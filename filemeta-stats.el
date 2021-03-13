;;; filemeta-stats.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(defun filemeta:db-dump (dir)
  "Expect DIR to be a filemeta-repo. Dump the db into an elisp
  list."
  (if (not (filemeta:is-repo-p dir))
      (error (format "(DIR=%s) must be a filemeta-repo." dir))
    (let* ((db (f-join dir filemeta:root-name))
           (hashdirs (f-directories db)))
      (loop for hashdir in hashdirs
            collect (list :hash (f-base hashdir)
                          :hashdir hashdir
                          :attachment
                          (let ((hash-file (concat hashdir "/"
                                                   filemeta:hashfile-name)))
                            (ignore-errors ;; TODO fihashdir this bad practice
                              (with-temp-buffer
                                (insert-file-contents hash-file)
                                (read (buffer-string))))))))))

(defun filemeta:hashes-in-repo (dir)
  "Expect DIR to be a filemeta-repo. Return all hashes in the
database."
  (mapcar (lambda (x) (plist-get x :hash))
          (filemeta:db-dump dir)))

(defun filemeta:tags-in-repo (dir)
  "Expect DIR to be a filemeta-repo. Return all tags in the
database."
  (sort (-uniq
         (-flatten
          (mapcar (lambda (x)
                    (plist-get (plist-get x :attachment) :tag))
                  (filemeta:db-dump dir))))
        #'string<))

(provide 'filemeta-stats)
