;;; filemeta-comment.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(defun filemeta-add-comment (filemeta comment)
  (if (stringp comment)
      (cl-pushnew comment (filemeta-comments filemeta) :test #'string=)
    (error "COMMENT must be a string.")))

(defun filemeta-remove-comment (filemeta comment)
  (setf (filemeta-comments filemeta)
        (remove comment (filemeta-comments filemeta))))

(defun filemeta-show-comments (filemeta)
  (filemeta-comments filemeta))

(defun filemeta-sort-comments (filemeta)
  (setf (filemeta-comments filemeta)
        (sort (filemeta-comments filemeta) 'string<)))

(defun filemeta-add-comment-to-file (file comment)
  "COMMENT is expected to be a string. FILE is expected to be a
pathname. .. ETC."
  (let* ((abs-file (file-truename file))
         (data-file (ensure-file-exists (concat *filemeta-root-dir* abs-file))) ;; deconstr!
         (content (f-read-text data-file 'utf-8))
         (data (ignore-errors (read content))))
    ;; if data is empty.. create a new filemeta for it
    ;; otherwise, add the tag for it.
    (if (eq data nil)
        (setf data (make-filemeta :path abs-file
                                  :comments (list comment)
                                  :tags nil
                                  :hists nil))
      (filemeta-add-comment data comment))
    ;; and then write the result back
    (write-region (prin1-to-string data) nil data-file)
    ))

(defun filemeta-remove-comment-from-file (file comment)
  "COMMENT is expected to be a string. FILE is expected to be a
pathname. .. ETC."
  (let* ((abs-file (file-truename file))
         (data-file (ensure-file-exists (concat *filemeta-root-dir* abs-file))) ;; deconstr!
         (content (f-read-text data-file 'utf-8))
         (data (ignore-errors (read content))))
    ;; if data is empty.. create a new filemeta for it
    ;; otherwise, add the tag for it.
    (if (eq data nil)
        (setf data (make-filemeta :path abs-file
                                  :comments nil
                                  :tags nil
                                  :hists nil))
      (filemeta-remove-comment data comment))
    ;; and then write the result back
    (write-region (prin1-to-string data) nil data-file)
    ))

(defun filemeta-add-comment-to-file-at-point ()
  ;; TODO add to multiple files?
  (interactive)
  (filemeta-add-comment-to-file
   (dired-get-filename)
   (ivy-read "Enter a comment: " nil)))

(defun filemeta-remove-comment-from-file-at-point ()
  ;; TODO add to multiple files?
  (interactive)
  (let* ((file (dired-get-filename))
         (data (filemeta-for-file file))
         (comments (filemeta-comments data))
         (c-to-remove (ivy-read "Select a comment to remove: " comments)))
    (filemeta-remove-comment-from-file file c-to-remove)))

(provide 'filemeta-comment)
