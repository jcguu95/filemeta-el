;; TODO This file is under construction.

(require 'f)

(defvar filemeta:root-name ".filemeta")
(defvar filemeta:hashfile-name ".db.el")

(defun filemeta:rel-path<-file (reg-file)
  (let ((root (filemeta:root<-file reg-file)))
    (concat "./" (f-relative reg-file root))))

(defun filemeta:init (dir)
  "Make the filemeta database for the current directory DIR."
  (let ((db (f-join dir filemeta:root-name)))
    (if (f-directory-p dir)
        (if (f-exists-p db)
            (error "Init process fails because DB exists.")
          (progn (mkdir db)
                 (f-write-text (prin1-to-string
                                (list (ts-format) "DB init."))
                               'utf-8
                               (f-join db "history"))))
      (error "DIR must be a directory."))))

(defun filemeta:root<-file (file)
  "It recursively searches upward for, and returns if any, the
  closest directory that contains \"filemeta\"."
  (labels ((parents (file)
                    "Return the list of parents for FILE recursively"
                    (unless (equal file "/")
                      (let ((parent (f-parent file)))
                        (cons parent (parents parent))))))
    (loop for d in (parents file)
          when (f-directory-p (concat d "/" filemeta:root-name))
          return d)))

(defun filemeta:hash<-file (file)
  "If FILE is a regular file, return the md5sum for its content.
  Otherwise, return nil."
  (flet ((md5sum (file)
                 (with-demoted-errors "Error: %S" ;; TODO what does this do exactly?
                   (and (file-exists-p file)
                        (with-temp-buffer
                          (insert-file-contents file)
                          (md5 (buffer-string)))))))
    (let ((size (f-size file))
          (thres filemeta:large-file-threshold))
      (when (> size thres)
        (message "Warning! \"%s\" is a large file (%s > %s)." file size thres)))
    (when (file-regular-p file)
      (md5sum file))))


(defun filemeta:hashdir<-file (file)
  "Return the path to the filemeta for the hash of FILE."
  (let ((hash (filemeta:hash<-file file))
        (root (filemeta:root<-file file)))
    (f-join root filemeta:root-name hash)))

(defun filemeta:hashfile<-file (file)
  "Return the path to the filemeta database for the hash of
FILE."
  (f-join (filemeta:hashdir<-file file) filemeta:hashfile-name))

(defun filemeta:filemeta<-file (file)
  "Expect a plist in the hash-file for the hash of FILE."
  (let ((hash-file (filemeta:hashfile<-file file)))
    (ignore-errors                       ;; TODO fix this bad practice
        (with-temp-buffer
          (insert-file-contents hash-file)
          (read (buffer-string))))))

(defun filemeta:write-filemeta! (x file)
  "Write X to the hash-file of FILE."
  ;; TODO Reformat the plist to be written by a variant of
  ;; #'lispy-multiline before writing.
  (let ((hash-dir (filemeta:hashdir<-file file))
        (hash-file (filemeta:hashfile<-file file)))
    (files--ensure-directory hash-dir)
    (f-write-text (prin1-to-string x)
                  'utf-8 hash-file)))

(defun filemeta:+tag! (tag file)
  "Expect TAG to be a symbol. Remove all tags that equal to TAG
  in the filemeta of FILE,and write the updated filemeta to the
  hash-file for FILE."
  (unless (symbolp tag)
    (error "TAG must be a symbol."))
  (flet ((sort+uniq (symbols)
                    (sort (-uniq symbols) #'string<)))
    (let* ((plist (filemeta:filemeta<-file file))
           (plist_ (plist-put plist     ;; TODO fix bad updating method..
                              :tag (sort+uniq
                                    (cons tag (plist-get plist :tag))))))
      (filemeta:write-filemeta! plist_ file))))

(defun filemeta:-tag! (tag file)
  "Expect TAG to be a symbol. Remove all tags that equal to TAG
  in the filemeta of FILE,and write the updated filemeta to the
  hash-file for FILE."
  (unless (symbolp tag)
    (error "TAG must be a symbol."))
  (flet ((sort+uniq (symbols)
                    (sort (-uniq symbols) #'string<)))
    (let* ((plist (filemeta:filemeta<-file file))
           (plist_ (plist-put plist ;; TODO fix bad updating method..
                              :tag (sort+uniq
                                    (-remove (lambda (x) (equal x tag))
                                             (plist-get plist :tag))))))
      (filemeta:write-filemeta! plist_ file))))

;;; hash history, relative path.. etc

(defun filemeta:update-file-history! (file)
  "Check and update the history of the hash of the FILE. Expect
FILE to be a regular file."
  (let* ((plist (filemeta:filemeta<-file file))
         (hist (plist-get plist :history))
         (rel-path (filemeta:rel-path<-file file))
         (last-rel-path (-last-item (-last-item hist))))
    ;; Update history slot accordingly.
    (if (equal rel-path last-rel-path)
        ;; Then only need to update time.
        (setf hist (append (-drop-last 1 hist)
                           `(,(list (ts-format) rel-path))))
      ;; Otherwise, add a new entry to history.
      (setf hist (append hist
                         `(,(list (ts-format) rel-path)))))
    ;; Update plist and write to database.
    (plist-put! plist :history hist)
    (filemeta:write-filemeta! plist file)))

;;; testing

(defvar filemeta:testdir "/tmp/filemeta/testing")
(defvar filemeta:testfile (f-join filemeta:testdir "hello.txt"))
(mkdir filemeta:testdir t)
(filemeta:init filemeta:testdir)
(f-write-text "" 'utf-8 filemeta:testfile)
(loop for tag in '(math physics cs nerdy techie)
      do (filemeta:+tag! tag filemeta:testfile))
(loop for tag in '(nerdy techie)
      do (filemeta:-tag! tag filemeta:testfile))

(filemeta:update-file-history! filemeta:testfile)
