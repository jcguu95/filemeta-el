;; TODO under construction

(require 'f)

(defvar filemeta-root-name ".filemeta")

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
    (concat root hash)))

(defun filemeta-hash-file (file)
  "Return the path to the filemeta database for the hash of
FILE."
  (concat (filemeta-hash-dir file) ".db.el"))

(defun filemeta-read-filemeta (file)
  ;; FIXME conflicts with the old one?
  ;; expect a FILEMETA to be read.
  ""
  (read (filemeta-hash-file file)))

(defun filemeta-write-filemeta (filemeta file)
  ;; FIXME conflicts with the old one?
  ""
  (if (filemeta-p filemeta)
      ;; (WRITE filemeta TO (filemeta-hash-file file))
      (error "Type error.")))

(defun filemeta-tag-file (tag file)
  "Expect TAG to be a symbol. Add tag to the filemeta of FILE,
and write the updated filemeta to the hash-file for FILE."
  ;; TODO ..
  )
