;;; vulpea-dblock.el --- Reactive vulpea dynamic blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ivris Raymond

;; Author: Ivris Raymond <theivris@pm.me>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (vulpea "2.0.0") (org "9.6"))
;; Keywords: outlines, org, notes
;; URL: https://github.com/Arenile/vulpea-dblock

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Declarative dynamic blocks for vulpea with reactive, incremental
;; refresh.  The vulpea database is the publisher, each block instance
;; in an open buffer is a subscriber, and only blocks whose query
;; results actually changed get re-rendered -- in small, resumable,
;; idle-time slices that never restart from scratch and never dirty a
;; buffer whose rendered output is unchanged.
;;
;; Block syntax:
;;
;;   #+BEGIN: vulpea :tags (paper toread) :todo t :sort mtime :reverse t :limit 20
;;   #+END:
;;
;; See the README for the full param table.  Legacy `node-list' blocks
;; keep working unchanged via `org-dblock-write:node-list', and
;; `vulpea-dblock-migrate-buffer' rewrites their headers in place.
;;
;; Enable with (vulpea-dblock-mode 1).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'vulpea)
(require 'vulpea-dblock-registry)
(require 'vulpea-dblock-render)
(require 'vulpea-dblock-scheduler)

;;; Org dynamic block writers

(defun vulpea-dblock--write (raw legacy)
  "Writer body shared by the `vulpea' and `node-list' block types.
RAW is the params plist org passes to the writer; LEGACY selects
`node-list' semantics.  Inserts the rendered content at point."
  (let* ((params (vulpea-dblock--normalize-params raw legacy))
         (self-id (and (or (eq (plist-get params :backlinks-to) 'self)
                           (eq (plist-get params :links-from) 'self))
                       (ignore-errors (org-entry-get nil "ID" t))))
         (notes (vulpea-dblock--run-query params self-id)))
    (insert (vulpea-dblock--render-string notes params))))

;;;###autoload
(defun org-dblock-write:vulpea (params)
  "Write a `vulpea' dynamic block for PARAMS."
  (vulpea-dblock--write params nil))

;;;###autoload
(defun org-dblock-write:node-list (params)
  "Write a legacy `node-list' dynamic block for PARAMS.
Thin alias preserving the semantics of the old hand-rolled writer,
including :format functions called as (FN TITLE ID TODO PRIORITY)."
  (vulpea-dblock--write params t))

;; Old org files sometimes name the ancient default formatter in :format.
(defalias 'org-roam-dblock--format-node #'vulpea-dblock--legacy-format-note)

;;; Publisher: database change events

(defvar vulpea-dblock--in-db-update nil
  "Non-nil while inside `vulpea-db-update-file'.
Suppresses the delete advice: updates delete-then-reinsert notes
internally, which must not surface as a deletion event.")

(defun vulpea-dblock--publish-change (path old new)
  "Build a change event for PATH from OLD and NEW note lists and publish it."
  (let (tags ids dests)
    (dolist (note (append old new))
      (setq tags (append (vulpea-note-tags note) tags))
      (push (vulpea-note-id note) ids)
      (setq dests (append (vulpea-dblock--note-link-dests note) dests)))
    (vulpea-dblock--publish
     (list :path path
           :tags (seq-uniq tags)
           :ids (seq-uniq ids)
           :dests (seq-uniq dests)))))

(defun vulpea-dblock--file-state (path)
  "Return the notes currently in the database for PATH, or nil.
Skipped entirely (returns nil) while a storm is being collapsed."
  (unless vulpea-dblock--storm
    (ignore-errors (vulpea-db-query-by-file-path path))))

(defun vulpea-dblock--db-update-file-advice (orig path &rest args)
  "Around advice on `vulpea-db-update-file': publish a change event.
Captures the db state for PATH before and after ORIG runs; ARGS are
passed through.  Publishing failures never break vulpea's sync."
  (let ((old (vulpea-dblock--file-state path)))
    (prog1 (let ((vulpea-dblock--in-db-update t))
             (apply orig path args))
      (condition-case err
          (vulpea-dblock--publish-change
           path old (vulpea-dblock--file-state path))
        (error
         (message "vulpea-dblock: publish failed for %s: %s"
                  path (error-message-string err)))))))

(defun vulpea-dblock--db-delete-file-advice (orig path &rest args)
  "Around advice on `vulpea-db--delete-file-notes': publish a deletion.
No-op while inside `vulpea-db-update-file', which calls the delete
internally as part of its delete-then-reinsert transaction."
  (if vulpea-dblock--in-db-update
      (apply orig path args)
    (let ((old (vulpea-dblock--file-state path)))
      (prog1 (apply orig path args)
        (condition-case err
            (vulpea-dblock--publish-change path old nil)
          (error
           (message "vulpea-dblock: publish failed for %s: %s"
                    path (error-message-string err))))))))

;;; Subscription lifecycle hooks

(defun vulpea-dblock--setup-buffer (&optional buffer)
  "Scan BUFFER (default current) and schedule its new/changed blocks.
No-op for buffers that are not eligible (non-file, non-org, or
outside `vulpea-db-sync-directories')."
  (with-current-buffer (or buffer (current-buffer))
    (when (vulpea-dblock--buffer-eligible-p)
      (add-hook 'kill-buffer-hook #'vulpea-dblock--forget-buffer nil t)
      (let ((changed (vulpea-dblock--scan-buffer)))
        (when changed
          (mapc #'vulpea-dblock--enqueue changed)
          (vulpea-dblock--schedule))))))

(defun vulpea-dblock--on-org-mode ()
  "Register blocks when an eligible org buffer is opened."
  (when (bound-and-true-p vulpea-dblock-mode)
    (vulpea-dblock--setup-buffer)))

(defun vulpea-dblock--on-after-save ()
  "Re-scan after save: picks up added/removed blocks and param edits."
  (when (and (bound-and-true-p vulpea-dblock-mode)
             (derived-mode-p 'org-mode))
    (vulpea-dblock--setup-buffer)))

;;; Manual commands

(defun vulpea-dblock--block-header-pos ()
  "Return bol of the vulpea/node-list block header containing point.
Point may be on the header, inside the body, or on the #+END line.
Returns nil when point is not in one of our blocks."
  (save-excursion
    (let ((origin (point))
          (case-fold-search t))
      (end-of-line)
      (catch 'found
        (while (re-search-backward org-dblock-start-re nil t)
          (forward-line 0)
          (when (vulpea-dblock--parse-header-at-point)
            (let ((header (point)))
              (save-excursion
                (forward-line 1)
                (when (and (re-search-forward org-dblock-end-re nil t)
                           (>= (pos-eol) origin))
                  (throw 'found header)))))
          (forward-line 0))
        nil))))

(defun vulpea-dblock--sub-at (pos)
  "Return the registered sub whose header sits at bol POS, or nil."
  (seq-find (lambda (sub) (eql (vulpea-dblock--marker-bol sub) pos))
            vulpea-dblock--buffer-subs))

(defun vulpea-dblock--refresh-at (pos)
  "Synchronously refresh the block whose header is at bol POS.
Uses the registered sub when one exists (keeping its result
signature warm); otherwise processes a transient, unregistered sub
through the same diff-write pipeline, so an up-to-date block never
dirties the buffer.  Returns the process outcome symbol."
  (let ((sub (vulpea-dblock--sub-at pos)))
    (if sub
        (prog1 (vulpea-dblock--process-sub sub 'force)
          (when (vulpea-dblock--sub-broken sub)
            (vulpea-dblock--handle-broken sub)))
      (pcase-let ((`(,name . ,raw)
                   (save-excursion
                     (goto-char pos)
                     (forward-line 0)
                     (vulpea-dblock--parse-header-at-point))))
        (let* ((marker (copy-marker pos))
               (params (vulpea-dblock--normalize-params
                        raw (string= name "node-list")))
               (transient (vulpea-dblock--sub-create
                           :id (gensym "vulpea-dblock-transient-")
                           :buffer (current-buffer)
                           :marker marker
                           :name name
                           :raw-params raw
                           :params params
                           :dep-keys '(:tags nil :targets nil :global t))))
          (unwind-protect
              (vulpea-dblock--process-sub transient 'force)
            (set-marker marker nil)))))))

;;;###autoload
(defun vulpea-dblock-refresh ()
  "Refresh the vulpea dynamic block at point, synchronously.
On other dynamic block types, falls back to `org-dblock-update'."
  (interactive)
  (let ((pos (vulpea-dblock--block-header-pos)))
    (cond
     (pos
      (pcase (vulpea-dblock--refresh-at pos)
        ('unchanged (message "vulpea-dblock: block already up to date"))
        ('updated (message "vulpea-dblock: block refreshed"))
        ('broken (user-error "vulpea-dblock: could not re-locate block"))
        (_ (message "vulpea-dblock: block refreshed"))))
     ((org-in-regexp org-dblock-start-re)
      (org-dblock-update))
     (t (user-error "No vulpea dynamic block at point")))))

;;;###autoload
(defun vulpea-dblock-refresh-buffer ()
  "Synchronously refresh every vulpea/node-list block in the buffer."
  (interactive)
  (when (bound-and-true-p vulpea-dblock-mode)
    (vulpea-dblock--setup-buffer))
  (let ((count 0)
        (headers nil))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-dblock-start-re nil t)
       (forward-line 0)
       (when (vulpea-dblock--parse-header-at-point)
         (push (point-marker) headers))
       (forward-line 1))
     (dolist (m (nreverse headers))
       (vulpea-dblock--refresh-at (marker-position m))
       (set-marker m nil)
       (cl-incf count)))
    (message "vulpea-dblock: refreshed %d block%s"
             count (if (= count 1) "" "s"))))

;;;###autoload
(defun vulpea-dblock-refresh-all ()
  "Mark every registered block dirty and drain the queue synchronously."
  (interactive)
  (unless (bound-and-true-p vulpea-dblock-mode)
    (user-error "Enable `vulpea-dblock-mode' first"))
  (vulpea-dblock--mark-all-dirty)
  (let ((n (length vulpea-dblock--queue)))
    (vulpea-dblock--drain)
    (message "vulpea-dblock: verified %d block%s" n (if (= n 1) "" "s"))))

;;; Migration

(defconst vulpea-dblock--migrate-key-map
  '((:order-by . :sort)
    (:empty-message . :empty))
  "Legacy param keys renamed by `vulpea-dblock-migrate-buffer'.")

(defun vulpea-dblock--migrate-params (raw)
  "Translate legacy node-list RAW params to the `vulpea' block syntax.
Returns a plist ready to be printed into a new header."
  (let* ((tags (vulpea-dblock--listify (plist-get raw :tags)))
         (tag (plist-get raw :tag))
         (any (eq (vulpea-dblock--unquote (plist-get raw :tags-match)) 'or))
         (out nil))
    (when tag (setq tags (cons tag tags)))
    (when tags
      (setq out (list (if any :tags-any :tags) tags)))
    (dolist (key '(:backlinks-to :links-from :todo :exclude-done
                   :priority :file :limit :reverse))
      (when-let* ((v (plist-get raw key)))
        (setq out (append out (list key v)))))
    (when (and (plist-get raw :todo-only) (not (plist-get raw :todo)))
      (setq out (append out (list :todo t))))
    (pcase-dolist (`(,old . ,new) vulpea-dblock--migrate-key-map)
      (when-let* ((v (plist-get raw old)))
        (setq out (append out (list new v)))))
    out))

(defun vulpea-dblock--format-header (name params)
  "Format a block header line for NAME with PARAMS (no indentation)."
  (concat "#+BEGIN: " name
          (let ((parts nil)
                (rest params))
            (while rest
              (push (format " %s %S" (car rest) (cadr rest)) parts)
              (setq rest (cddr rest)))
            (apply #'concat (nreverse parts)))))

;;;###autoload
(defun vulpea-dblock-migrate-buffer ()
  "Rewrite #+BEGIN: node-list headers in this buffer to #+BEGIN: vulpea.
Param names are translated (:tag/:tags-match, :order-by,
:empty-message, :todo-only).  Blocks with a :format function are
skipped: legacy formatters take (TITLE ID TODO PRIORITY) while
`vulpea' formatters take a note, so they keep working as node-list
blocks instead."
  (interactive)
  (let ((migrated 0)
        (skipped 0))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-dblock-start-re nil t)
       (forward-line 0)
       (pcase (vulpea-dblock--parse-header-at-point)
         (`("node-list" . ,raw)
          (if (plist-get raw :format)
              (cl-incf skipped)
            (let ((indent (buffer-substring-no-properties
                           (pos-bol)
                           (progn (back-to-indentation) (point)))))
              (delete-region (pos-bol) (pos-eol))
              (insert indent
                      (vulpea-dblock--format-header
                       "vulpea" (vulpea-dblock--migrate-params raw)))
              (cl-incf migrated)))))
       (forward-line 1)))
    (when (bound-and-true-p vulpea-dblock-mode)
      (vulpea-dblock--setup-buffer))
    (message "vulpea-dblock: migrated %d block%s%s"
             migrated (if (= migrated 1) "" "s")
             (if (> skipped 0)
                 (format ", skipped %d with custom :format" skipped)
               ""))))

;;; Report

;;;###autoload
(defun vulpea-dblock-report ()
  "Show per-block last query/render durations and registry state.
Use this to find pathologically slow blocks: the scheduler never
aborts a block mid-render, so a slow query here is what a visible
hitch looks like."
  (interactive)
  (let ((subs (sort (vulpea-dblock--all-subs)
                    (lambda (a b)
                      (> (or (vulpea-dblock--sub-last-query-ms a) 0)
                         (or (vulpea-dblock--sub-last-query-ms b) 0))))))
    (with-help-window "*vulpea-dblock report*"
      (princ (format "%d registered block(s), %d queued, storm: %s\n\n"
                     (length subs)
                     (length vulpea-dblock--queue)
                     (if vulpea-dblock--storm "yes" "no")))
      (princ (format "%-28s %8s %10s %11s %s\n"
                     "BUFFER" "POS" "QUERY(ms)" "RENDER(ms)" "PARAMS"))
      (dolist (sub subs)
        (let ((buf (vulpea-dblock--sub-buffer sub)))
          (princ (format "%-28s %8s %10s %11s %s%s\n"
                         (if (buffer-live-p buf) (buffer-name buf) "<dead>")
                         (or (vulpea-dblock--marker-bol sub) "?")
                         (if-let* ((ms (vulpea-dblock--sub-last-query-ms sub)))
                             (format "%.1f" ms) "-")
                         (if-let* ((ms (vulpea-dblock--sub-last-render-ms sub)))
                             (format "%.1f" ms) "-")
                         (if (vulpea-dblock--sub-dirty sub) "[dirty] " "")
                         (prin1-to-string
                          (vulpea-dblock--sub-raw-params sub)))))))))

;;; Minor mode

(defun vulpea-dblock--enable ()
  "Install advice and hooks, then register blocks in open org buffers."
  (advice-add 'vulpea-db-update-file :around
              #'vulpea-dblock--db-update-file-advice)
  (advice-add 'vulpea-db--delete-file-notes :around
              #'vulpea-dblock--db-delete-file-advice)
  (add-hook 'org-mode-hook #'vulpea-dblock--on-org-mode)
  (add-hook 'after-save-hook #'vulpea-dblock--on-after-save)
  (add-hook 'kill-emacs-hook #'vulpea-dblock--shutdown)
  (dolist (buf (buffer-list))
    (when (vulpea-dblock--buffer-eligible-p buf)
      (vulpea-dblock--setup-buffer buf))))

(defun vulpea-dblock--disable ()
  "Remove advice, hooks and timers; clear the registry."
  (advice-remove 'vulpea-db-update-file
                 #'vulpea-dblock--db-update-file-advice)
  (advice-remove 'vulpea-db--delete-file-notes
                 #'vulpea-dblock--db-delete-file-advice)
  (remove-hook 'org-mode-hook #'vulpea-dblock--on-org-mode)
  (remove-hook 'after-save-hook #'vulpea-dblock--on-after-save)
  (remove-hook 'kill-emacs-hook #'vulpea-dblock--shutdown)
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'vulpea-dblock--buffer-subs buf)
      (with-current-buffer buf
        (remove-hook 'kill-buffer-hook #'vulpea-dblock--forget-buffer t))))
  (vulpea-dblock--shutdown)
  (vulpea-dblock--registry-clear))

;;;###autoload
(define-minor-mode vulpea-dblock-mode
  "Global minor mode: reactive refresh for vulpea dynamic blocks.

While enabled, every `vulpea' and `node-list' dynamic block in an
open org buffer under `vulpea-db-sync-directories' is subscribed to
vulpea database changes.  When the database ingests a change (save,
git pull, Syncthing, full scan), only the blocks whose dependencies
match are re-verified, and only blocks whose rendered output really
changed are rewritten -- in idle-time slices bounded by
`vulpea-dblock-tick-budget' that resume instead of restarting."
  :global t
  :group 'vulpea-dblock
  (if vulpea-dblock-mode
      (vulpea-dblock--enable)
    (vulpea-dblock--disable)))


;;;###autoload
(defun vulpea-dblock-insert-dblock ()
  "Insert a vulpea dynamic block at point."
  (interactive)
  (org-create-dblock (list :name "vulpea" :tags '() :sort 'mtime :reverse t :limit 10))
  (org-update-dblock))

;; org is a hard dependency (via vulpea), so it is already loaded here
(when (fboundp 'org-dynamic-block-define)
  (org-dynamic-block-define "vulpea"
                            #'vulpea-dblock-insert-dblock))


(provide 'vulpea-dblock)
;;; vulpea-dblock.el ends here
