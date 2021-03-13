;;; filemeta-dired.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(defun filemeta-dired-sort-tags-for-marked-files ()
  "Deconstructively sort tags for the marked files, or the file
at point if no files are marked."
  (interactive)
  (loop for file in (dired-get-marked-files) do
        (filemeta-sort-tags-for-file file)))

(defun filemeta-dired-add-tags-to-marked-files ()
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

(defun filemeta-dired-remove-tags-from-marked-files ()
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

(provide 'filemeta-dired)
