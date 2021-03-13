;;; filemeta-dired.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(defun filemeta:dired-marked-files-attachments ()
  "Return the attachments of all marked files."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (loop for file in files
          collect (filemeta:attachment<-file file))))

(defun filemeta:dired-marked-files-tags ()
  "Return all tags that appear in some marked files."
  (interactive)
  (-uniq (-flatten
    (mapcar (lambda (x) (plist-get x :tag))
            (filemeta:dired-marked-files-attachments)))))

(defun filemeta:dired-marked-files-+tag ()
  "Let user add tag(s) to marked files in dired."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (raw-tags (ivy-read "+tags: " (filemeta:tags-in-repo ".")))
         (tags (mapcar #'intern (split-string raw-tags))))
    (loop for file in files
          do (progn (filemeta:update-file-history! file)
                    (loop for tag in tags
                          do (filemeta:+tag! tag file))))))

(defun filemeta:dired-marked-files--tag ()
  "Let user remove tag(s) from marked files in dired."
  (interactive)
  (let* ((files (dired-get-marked-files))
         ;; FIXME only have to show tags for the marked files
         (raw-tags (ivy-read "+tags: " (filemeta:dired-marked-files-tags)))
         (tags (mapcar #'intern (split-string raw-tags))))
    (loop for file in files
          do (progn (filemeta:update-file-history! file)
                    (loop for tag in tags
                          do (filemeta:-tag! tag file))))))

(provide 'filemeta-dired)
