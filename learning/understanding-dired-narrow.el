(defun dired-narrow--string-filter (filter)
  (let ((words (split-string filter " ")))
    (--all? (save-excursion (search-forward it (line-end-position) t)) words)))

;; this is very abstract to me
;; ask #emacs for help tomorrow

(defun dired-narrow ()
  "Narrow a dired buffer to the files matching a string.

If the string contains spaces, then each word is matched against
the file name separately.  To succeed, all of them have to match
but the order does not matter.

For example \"foo bar\" matches filename \"bar-and-foo.el\"."
  (interactive)
  (dired-narrow--internal 'dired-narrow--string-filter))

(defun dired-narrow--internal (filter-function)
  "Narrow a dired buffer to the files matching a filter.

The function FILTER-FUNCTION is called on each line: if it
returns non-nil, the line is kept, otherwise it is removed.  The
function takes one argument, which is the current filter string
read from minibuffer."
  (let ((dired-narrow-buffer (current-buffer))
        (dired-narrow-filter-function filter-function)
        (disable-narrow nil))
    (unwind-protect
        (progn
          (dired-narrow-mode 1)
          (add-to-invisibility-spec :dired-narrow)
          (setq disable-narrow (read-from-minibuffer "Filter: " nil dired-narrow-map))
          (let ((inhibit-read-only t))
            (dired-narrow--remove-text-with-property :dired-narrow))
          ;; If the file no longer exists, we can't do anything, so
          ;; set to nil
          (unless (dired-utils-goto-line dired-narrow--current-file)
            (setq dired-narrow--current-file nil)))
      (with-current-buffer dired-narrow-buffer
        (unless disable-narrow (dired-narrow-mode -1))
        (remove-from-invisibility-spec :dired-narrow)
        (dired-narrow--restore))
      (when (and disable-narrow
                 dired-narrow--current-file
                 dired-narrow-exit-action)
        (funcall dired-narrow-exit-action))
      (cond
       ((equal disable-narrow "dired-narrow-enter-directory")
        (dired-narrow--internal filter-function))))))
