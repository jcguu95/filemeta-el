;;; filemeta-tag.el -mode -*- coding: utf-8; lexical-binding: t; -*-

;;; functional

(defun filemeta-show-tags (filemeta)
  (filemeta-tags filemeta))

(defun filemeta-file-has-tag-p (file tag)
  (member tag (filemeta-tags (filemeta-for-file file))))

(defun filemeta-file-has-tag-at-point-p ()
  (interactive)
  (let ((tag (intern (ivy-read "Enter tag: " nil))))
    (if (filemeta-file-has-tag-p (dired-get-filename) tag)
        (message "true")
      (message "false"))))

;;; sort tags

(defun filemeta-sort-tags (filemeta)
  "Functionally sort the tags of FILEMETA and return a fresh
copy. FILEMETA untouched."
  (let ((result (copy-filemeta filemeta)))
    ;; TODO Can I do it functionally without explicitly copy the
    ;; whole struct?
    (setf (filemeta-tags result)
          (-sort 'string< (filemeta-tags filemeta)))
    result))

(defun filemeta-sort-tags-for-file (file)
  "Deconstructively sort the tags for the filemeta of the FILE."
  (let* ((data-file (filemeta-data-for-file--ensured file))
         (filemeta (filemeta-filemeta-from-data-file data-file)))
    (filemeta-write-filemeta-to-data-file (filemeta-sort-tags filemeta) data-file)))

(defun filemeta-sort-tags-for-file-at-point ()
  (interactive)
  (filemeta-sort-tags-for-file (dired-get-filename)))

;;; add tags

(defun filemeta-add-tag (filemeta symbol)
  ;; e.g. (filemeta-add-tag filemeta-eg1 'math)
  (if (symbolp symbol)
      (cl-pushnew symbol (filemeta-tags filemeta))
    (error "SYMBOL must be a symbol.")))

(defun filemeta-add-tag-to-file (file tag)
  "TAG is expected to be a symbol. FILE is expected to be a
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
                                  :tags (list tag)
                                  :hists nil))
      (filemeta-add-tag data tag))
    ;; and then write the result back
    (write-region (prin1-to-string data) nil data-file)))

(defun filemeta-add-tag-to-file-at-point ()
  ;; TODO add doc string
  ;; TODO add to multiple files?
  (interactive)
  (let* ((tags-string (ivy-read "Enter tags: " nil)) ;; TODO read candidates from a tag db
         (tags (mapcar #'intern (split-string (s-collapse-whitespace tags-string))))) ;; tokenize the tags
    (loop for tag in tags do
          (filemeta-add-tag-to-file (dired-get-filename) tag))))

;;; remove tags

(defun filemeta-remove-tag (filemeta symbol)
  ;; e.g. (filemeta-remove-tag filemeta-eg1 'math)
  (setf (filemeta-tags filemeta)
        (remove symbol (filemeta-tags filemeta))))

(defun filemeta-remove-tag-from-file (file tag)
  "TAG is expected to be a symbol. FILE is expected to be a
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
      (filemeta-remove-tag data tag))
    ;; and then write the result back
    (write-region (prin1-to-string data) nil data-file)))

(defun filemeta-remove-tag-from-file-at-point ()
  ;; TODO add to multiple files?
  (interactive)
  (let* ((file (dired-get-filename))
         (data (filemeta-for-file file))
         (tags (filemeta-tags data))
         (tags-string (ivy-read "Select tag to remove: " tags))
         (tags-to-remove (mapcar #'intern (split-string (s-collapse-whitespace tags-string)))))
    (loop for tag-to-remove in tags-to-remove do
          (filemeta-remove-tag-from-file file tag-to-remove))))

(provide 'filemeta-tag)
