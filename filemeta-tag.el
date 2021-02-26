;;; filemeta-tag.el -mode -*- coding: utf-8; lexical-binding: t; -*-

;;; sort tags

(defun filemeta-sort-tags (filemeta)
  "Functionally sort the tags of FILEMETA and return a fresh
copy. FILEMETA untouched."
  (let ((result (copy-filemeta filemeta)))
    (setf (filemeta-tags result)
          (-sort 'string< (filemeta-tags filemeta)))
    result))

(defun filemeta-sort-tags-for-file (file)
  "Deconstructively sort the tags for the filemeta of the FILE."
  (let* ((data-file (filemeta-data-for-file--ensured file))
         (filemeta (filemeta-filemeta-from-data-file data-file)))
    (filemeta-write-filemeta-to-data-file
     (filemeta-sort-tags filemeta) data-file)))

(defun filemeta-sort-tags-for-marked-files ()
  "Deconstructively sort tags for the marked files, or the file
at point if no files are marked."
  (interactive)
  (loop for file in (dired-get-marked-files) do
       (filemeta-sort-tags-for-file file)))

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

(defun filemeta-add-tags-to-marked-files ()
  "In dired-mode, ask the user to input a string STR. Tokenize
the string to a list of tags. Deconstructively add the tags to
the data files of all the marked files, or the file at point if
no files are marked."
  (interactive)
  (let* ((str (ivy-read "Add tags: " nil)) ;; TODO read candidates from a tag db
         (tags-to-add (filemeta-tokenize str)))
    (loop for file in (dired-get-marked-files) do
          (loop for tag in tags-to-add do
                (filemeta-add-tag-to-file file tag)))))

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

(defun filemeta-remove-tags-from-marked-files ()
  "In dired-mode, ask the user to input a string STR. Tokenize
the string to a list of tags. Deconstructively remove the tags
from the data file of the file at point."
  (interactive)
  (let* ((marked-files (dired-get-marked-files))
         (tags (-uniq (flatten-list
                       (loop for file in marked-files collect
                             (filemeta-tags (filemeta-for-file file))))))
         (str (ivy-read "Remove tags: " tags))
         (tags-to-remove (filemeta-tokenize str)))
    (loop for file in marked-files do
          (loop for tag in tags-to-remove do
                (filemeta-remove-tag-from-file file tag)))))

;; stats

(defun filemeta-ls-tags ()
  "List all tags under *FILEMETA-ROOT-DIR*."
  ;; TODO Take care real NILs and fake NILs.
  (-uniq (flatten-list
          (loop for data-file in (directory-files-recursively *filemeta-root-dir* "")
                collect (let ((filemeta (filemeta-filemeta-from-data-file data-file)))
                          (when filemeta (filemeta-tags filemeta)))))))

(defun filemeta-ls-files ()
  "Return all files that are associated with some data files
  under *FILEMETA-ROOT-DIR*."
  (remove nil
          (loop for data-file in (directory-files-recursively *filemeta-root-dir* "") ;; TODO use filemeta-ls-data-files
                collect (let ((filemeta (filemeta-filemeta-from-data-file data-file)))
                          (when filemeta (filemeta-path filemeta))))))

(defun filemeta-ls-data-files ()
  "List all data files under *FILEMETA-ROOT-DIR*."
  (directory-files-recursively *filemeta-root-dir* ""))

(defun filemeta-has-tag-p (filemeta tag)
  "Return whether FILEMETA has TAG."
  (when (member tag (filemeta-tags filemeta)) t))

(defun filemeta-has-tags-p (filemeta tags)
  "Return whether FILEMETA has all tags in TAGS."
  (eval (cons 'and ;; weird code cuz 'and is a macro.. can't apply it directly.
              (loop for tag in tags collect
                    (when (member tag (filemeta-tags filemeta)) t)))))

(defun filemeta-ls-files-having-tag (tag)
  "List all files that have tag TAG."
  (-filter (lambda (file)
             (filemeta-has-tag-p (filemeta-for-file file) tag))
           (filemeta-ls-files)))

(defun filemeta-ls-files-having-tags (tags)
  "List all files that have all tags in TAGS."
  (-filter (lambda (file)
             (filemeta-has-tags-p (filemeta-for-file file) tags))
           (filemeta-ls-files)))

(defun filemeta-count-files-having-tag (tag)
  "Count the amount of files under *FILEMETA-ROOT-DIR* that have
  the tag TAG."
  (length (filemeta-ls-files-having-tag tag)))

(provide 'filemeta-tag)
