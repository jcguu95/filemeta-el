;; TODO under construction
;;
;; TODO basic ingredients are made.. now try to merge the old
;; functions here.
;;
;; FIXME the filename "filemeta-root" is a misnomer.. fix it.

(require 'f)

(defvar filemeta-root-name ".filemeta")

(defun filemeta-init (path)
  "Make the filemeta database for the current directory PATH."
  (let ((db (f-join path filemeta-root-name)))
    (if (f-directory-p path)
        (if (f-exists-p db)
            (error "Init process fails because DB exists.")
          (progn (mkdir db)
                 (f-write-text (prin1-to-string (cons (ts-format) "Db init."))
                               'utf-8 (f-join db "history"))))
      (error "PATH must be a directory."))))

(defun filemeta-wheres-root (path)
  "It recursively searches upward for, and returns if any, the
  closest directory that contains \"filemeta\"."
  (labels ((parents (path)
                    "Return the list of parents for PATH recursively"
                    (unless (equal path "/")
                      (let ((parent (f-parent path)))
                        (cons parent (parents parent))))))
    (loop for d in (parents path)
          when (f-directory-p (concat d "/" filemeta-root-name))
          return d)))

(defun filemeta-path-hash (path)
  "If PATH is a regular file, return the md5sum for its content.
  Otherwise, return nil."
  (flet ((md5sum (file)
                 (with-demoted-errors "Error: %S" ;; TODO what does this do exactly?
                   (and (file-exists-p file)
                        (with-temp-buffer
                          (insert-file-contents file)
                          (md5 (buffer-string)))))))
    (when (file-regular-p path)
      (md5sum path))))

(defun filemeta-hash-dir (file)
  "Return the path to the filemeta for the hash of FILE."
  (let ((hash (filemeta-path-hash file))
        (root (filemeta-wheres-root file)))
    (f-join root filemeta-root-name hash)))

(defun filemeta-hash-file (file)
  "Return the path to the filemeta database for the hash of
FILE."
  (f-join (filemeta-hash-dir file) ".db.el"))

(defun filemeta-read-filemeta (file)
  "Expect a plist in the hash-file for the hash of FILE."
  (let ((hash-file (filemeta-hash-file file)))
    (ignore-errors                       ;; TODO fix this bad practice
        (with-temp-buffer
          (insert-file-contents hash-file)
          (read (buffer-string))))))

(defun filemeta-write-filemeta (x file)
  "Write X to the hash-file of FILE."
  (let ((hash-dir (filemeta-hash-dir file))
        (hash-file (filemeta-hash-file file)))
    (files--ensure-directory hash-dir)
    (f-write-text (prin1-to-string x)
                  'utf-8 hash-file)))

(defun filemeta-add-tag-to-file (tag file)
  "Expect TAG to be a symbol. Add tag to the filemeta of FILE,
and write the updated filemeta to the hash-file for FILE."
  (unless (symbolp tag)
    (error "TAG must be a symbol."))
  (let* ((plist (filemeta-read-filemeta file)))
    (filemeta-write-filemeta
     (plist-put plist
                :tag (sort (-uniq (cons tag
                                        (plist-get plist :tag)))
                           #'string<))
     file)
    ))

(defun filemeta-remove-tag-from-file (tag file)
  "Expect TAG to be a symbol. Remove all tags that equal to TAG
  in the filemeta of FILE,and write the updated filemeta to the
  hash-file for FILE."
  (unless (symbolp tag)
    (error "TAG must be a symbol."))
  (flet ((sort+uniq (symbols)
                    (sort (-uniq symbols) #'string<)))
    (let* ((plist (filemeta-read-filemeta file))
           (plist_ (plist-put plist     ;; TODO fix bad updating method..
                              :tag (sort+uniq (-remove (lambda (x) (equal x tag))
                                                       (plist-get plist :tag))))))
      (filemeta-write-filemeta plist_ file))))

;;; testing
(filemeta-add-tag-to-file 'bch "~/testing/hi.txt")
(filemeta-remove-tag-from-file 'bch "~/testing/hi.txt")
