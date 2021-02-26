;;; filemeta-tag.el -mode -*- coding: utf-8; lexical-binding: t; -*-

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

(defun filemeta-add-tag (filemeta tag)
  "Expect TAG to be a symbol. Functionally add TAG to the tag
slot of FILEMETA, without touching FILEMETA."
  (let ((result (copy-filemeta filemeta)))
    (if (symbolp tag)
        (cl-pushnew tag (filemeta-tags result))
      (error "TAG must be a symbol."))
    result))

(defun filemeta-add-tag-to-file (file tag)
  "Deconstructively push a single symbol TAG into the tags-slot
of the filemeta of a single file FILE."
  (let* ((data-file (filemeta-data-for-file--ensured file))
         (filemeta (filemeta-filemeta-from-data-file data-file)))
    (filemeta-write-filemeta-to-data-file
     (if (eq filemeta nil)
         (make-filemeta :path (file-truename file) :comments nil
                        :tags (list tag) :hists nil)
       (filemeta-add-tag filemeta tag))
     data-file)))

(defun filemeta-add-tags-to-file-at-point ()
  "In dired-mode, ask the user to input a string STR. Tokenize
the string to a list of tags. Deconstructively add the tags to
the data file of the file at point."
  (interactive)
  (let* ((file (dired-get-filename))
         (str (ivy-read "Add tags: " nil)) ;; TODO read candidates from a tag db
         (tags-to-add (filemeta-tokenize str))) ;; TODO add to multiple files?
    (loop for tag in tags-to-add do
          (filemeta-add-tag-to-file file tag))))

;;; remove tags

(defun filemeta-remove-tag (filemeta tag)
  "Expect TAG to be a symbol. Functionally remove TAG from the
tag slot of FILEMETA, without touching FILEMETA."
  (let ((result (copy-filemeta filemeta)))
    (if (symbolp tag)
        (setf (filemeta-tags result)
              (remove tag (filemeta-tags filemeta)))
      (error "TAG must be a symbol."))
    result))

(defun filemeta-remove-tag-from-file (file tag)
  "Deconstructively remove a single symbol TAG into the tags-slot
of the filemeta of a single file FILE."
  (let* ((data-file (filemeta-data-for-file--ensured file))
         (filemeta (filemeta-filemeta-from-data-file data-file)))
    (filemeta-write-filemeta-to-data-file
     (if (eq filemeta nil) nil
       (filemeta-remove-tag filemeta tag))
     data-file)))

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

(defun filemeta-remove-tags-from-file-at-point ()
  "In dired-mode, ask the user to input a string STR. Tokenize
the string to a list of tags. Deconstructively remove the tags
from the data file of the file at point."
  (interactive)
  (let* ((file (dired-get-filename)) ;; TODO add to multiple files?
         (tags (filemeta-tags (filemeta-for-file file)))
         (str (ivy-read "Remove tags: " tags)) ;; TODO read candidates from a tag db
         (tags-to-remove (filemeta-tokenize str)))
    (loop for tag in tags-to-remove do
          (filemeta-remove-tag-from-file file tag))))

(provide 'filemeta-tag)
