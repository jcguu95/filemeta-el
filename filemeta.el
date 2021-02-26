;;; filemeta.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(require 'filemeta-comment)
(require 'filemeta-tag)

(defstruct filemeta
  path comments tags hists) ;; TODO add time for comments

(setq filemeta-eg1
      (make-filemeta
       :path "~/test-image/1.jpg"
       :comments '("I love this photo.")
       :tags '(college math career)
       :hists nil))

(mkdir (setq *filemeta-root-dir* "/tmp/filemeta") t)

(defun ensure-file-exists (file)
  ;; https://stackoverflow.com/questions/16397259/ensure-filepath-exists-for-reading
  (unless (file-exists-p file)
    (make-directory (file-name-directory file) t)
    (write-region "" nil file nil 'silent))
  file)

(defun filemeta-truename (filemeta)
  (file-truename (filemeta-path filemeta)))
(defun filemeta-for-file (file)
  (let* ((abs-file (file-truename file))
         (data-file (ensure-file-exists (concat *filemeta-root-dir* abs-file))) ;; deconstr!
         (content (f-read-text data-file 'utf-8))
         (data (ignore-errors (read content))))
    data))

(defun filemeta-for-file-at-point ()
  ;; TODO add to multiple files?
  (interactive)
  (print (filemeta-for-file (dired-get-filename))))
