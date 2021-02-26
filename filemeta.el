;;; filemeta.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(require 'filemeta-tag)
(require 'filemeta-comment)
(require 'filemeta-health)

(defstruct filemeta
  path comments tags hists) ;; TODO add time for comments

(setq filemeta-eg1
      (make-filemeta
       :path "~/test-image/1.jpg"
       :comments '("I love this photo.")
       :tags '(college math career)
       :hists nil))

(mkdir (setq *filemeta-root-dir* "/tmp/filemeta") t)

(defun filemeta-ensure-file-exists (file)
  ;; https://stackoverflow.com/questions/16397259/ensure-filepath-exists-for-reading
  (unless (file-exists-p file)
    (make-directory (file-name-directory file) t)
    (write-region "" nil file nil 'silent))
  file)

(defun filemeta-truename (filemeta)
  (file-truename (filemeta-path filemeta)))
(defun filemeta-for-file (file)
  (let* ((abs-file (file-truename file))
         (data-file (filemeta-ensure-file-exists (concat *filemeta-root-dir* abs-file))) ;; deconstr!
         (content (f-read-text data-file 'utf-8))
         (data (ignore-errors (read content))))
    data))

(defun filemeta-for-file-at-point ()
  ;; TODO add to multiple files?
  (interactive)
  (print (filemeta-for-file (dired-get-filename))))

(defun filemeta-data-for-file (file)
  "An function utility that returns the path to the data file of
the input FILE."
  (concat *filemeta-root-dir* (file-truename file)))

(defun filemeta-data-for-file--ensured (file)
  "An utility that deconstructively ensures and gets the path of
the data file for the input FILE. It creates the data file, if
not exists yet, under *FILEMETA-ROOT-DIR* recursively."
  (filemeta-ensure-file-exists (filemeta-data-for-file file)))

(defun filemeta-filemeta-from-data-file (data-file)
  "An utility that reads the DATA-FILE and expects the content
inside records a filemeta. If there's any error, return NIL."
  (ignore-errors (read (f-read-text data-file 'utf-8))))

(defun filemeta-write-filemeta-to-data-file (filemeta data-file)
  "An utility that overwrites the FILEMETA to the DATAFILE."
  (message (format "Updating %s.." data-file))
  (write-region (prin1-to-string filemeta) nil data-file))

(defun filemeta-tokenize (str)
  "A utility that tokenize the input string STR into a list of
symbols."
  (mapcar #'intern (split-string (s-collapse-whitespace str))))
