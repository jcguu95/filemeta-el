;;; filemeta.el -mode -*- coding: utf-8; lexical-binding: t; -*-

;; TODO list
;;
;; 1. Separate files into smaller ones: inspect, comment, tag, utils.
;; 2. More doc strings! More message output!
;; 3. integrate with dired narrow
;; 4. #'filemeta-undo
;; 5. Add a nice filemeta shower.
;; 6. Add a tag statistics reporter.
;; 7. Add a canonical annotating folder and org file to each file.
;; 8. Add an org/html rendered -- eg Render an html with links and even with embedded images with all files that have tag 'x and have no tag 'y.
;; 9. Add a db health checker/ hash checker.
;; 10. Automatic git the db while a destructive action is made.
;; 11. Ask help for "mark" all files in dired that satisfy abc..
;; 12. filemeta-history: a complete history of what has been done


;;;;;; structures ;;;;;;

(defstruct filemeta
  path comments tags hists) ;; TODO add time for comments

(setq filemeta-eg1
      (make-filemeta
       :path "~/test-image/1.jpg"
       :comments '("I love this photo.")
       :tags '(college math career)
       :hists nil))

(mkdir (setq *filemeta-root-dir* "/tmp/filemeta") t)

;;;;;; functions ;;;;;;

;; util
(defun ensure-file-exists (file)
  ;; https://stackoverflow.com/questions/16397259/ensure-filepath-exists-for-reading
  (unless (file-exists-p file)
    (make-directory (file-name-directory file) t)
    (write-region "" nil file nil 'silent))
  file)

(defun filemeta-truename (filemeta)
  (file-truename (filemeta-path filemeta)))

;; comment
(defun filemeta-add-comment (filemeta comment)
  (if (stringp comment)
      (cl-pushnew comment (filemeta-comments filemeta) :test #'string=)
    (error "COMMENT must be a string.")))

(defun filemeta-remove-comment (filemeta comment)
  (setf (filemeta-comments filemeta)
        (remove comment (filemeta-comments filemeta))))

(defun filemeta-show-comments (filemeta)
  (filemeta-comments filemeta))

(defun filemeta-sort-comments (filemeta)
  (setf (filemeta-comments filemeta)
        (sort (filemeta-comments filemeta) 'string<)))

;; tag
(defun filemeta-add-tag (filemeta symbol)
  ;; e.g. (filemeta-add-tag filemeta-eg1 'math)
  (if (symbolp symbol)
      (cl-pushnew symbol (filemeta-tags filemeta))
      (error "SYMBOL must be a symbol.")))

(defun filemeta-remove-tag (filemeta symbol)
  ;; e.g. (filemeta-remove-tag filemeta-eg1 'math)
  (setf (filemeta-tags filemeta)
        (remove symbol (filemeta-tags filemeta))))

(defun filemeta-show-tags (filemeta)
  ;; e.g. (filemeta-show-tags filemeta-eg1)
  (filemeta-tags filemeta))

(defun filemeta-sort-tags (filemeta)
  (setf (filemeta-tags filemeta)
        (sort (filemeta-tags filemeta) 'string<)))

(defun filemeta-add-tag-to-file (file tag)
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
                                  :tags (list tag)
                                  :hists nil))
      (filemeta-add-tag data tag))
    ;; and then write the result back
    (write-region (prin1-to-string data) nil data-file)))

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
    (write-region (prin1-to-string data) nil data-file)
    ))

(defun filemeta-add-comment-to-file (file comment)
  "COMMENT is expected to be a string. FILE is expected to be a
pathname. .. ETC."
  (let* ((abs-file (file-truename file))
         (data-file (ensure-file-exists (concat *filemeta-root-dir* abs-file))) ;; deconstr!
         (content (f-read-text data-file 'utf-8))
         (data (ignore-errors (read content))))
    ;; if data is empty.. create a new filemeta for it
    ;; otherwise, add the tag for it.
    (if (eq data nil)
        (setf data (make-filemeta :path abs-file
                                  :comments (list comment)
                                  :tags nil
                                  :hists nil))
      (filemeta-add-comment data comment))
    ;; and then write the result back
    (write-region (prin1-to-string data) nil data-file)
    ))

(defun filemeta-remove-comment-from-file (file comment)
  "COMMENT is expected to be a string. FILE is expected to be a
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
      (filemeta-remove-comment data comment))
    ;; and then write the result back
    (write-region (prin1-to-string data) nil data-file)
    ))

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

(defun filemeta-file-has-tag-p (file tag)
  (member tag (filemeta-tags (filemeta-for-file file))))

(defun filemeta-file-has-tag-at-point-p ()
  (interactive)
  (let ((tag (intern (ivy-read "Enter tag: " nil))))
    (if (filemeta-file-has-tag-p (dired-get-filename) tag)
        (message "true")
      (message "false"))))

(defun filemeta-add-tag-to-file-at-point ()
  ;; TODO add doc string
  ;; TODO add to multiple files?
  (interactive)
  (let* ((tags-string (ivy-read "Enter tags: " nil)) ;; TODO read candidates from a tag db
         (tags (mapcar #'intern (split-string (s-collapse-whitespace tags-string))))) ;; tokenize the tags
    (loop for tag in tags do
          (filemeta-add-tag-to-file (dired-get-filename) tag))))

(defun filemeta-remove-tag-from-file-at-point ()
  ;; TODO add to multiple files?
  (interactive)
  (let* ((file (dired-get-filename))
         (data (filemeta-for-file file))
         (tags (filemeta-tags data))
         (tags-string (ivy-read "Select tag to remove: " tags))
         (tags-to-remove (mapcar #'intern (split-string (s-collapse-whitespace tags-string)))))
    (loop for tag-to-remove in tags-to-remove do
          (filemeta-remove-tag-from-file file tag-to-remove))))

(defun filemeta-add-comment-to-file-at-point ()
  ;; TODO add to multiple files?
  (interactive)
  (filemeta-add-comment-to-file
   (dired-get-filename)
   (ivy-read "Enter a comment: " nil)))

(defun filemeta-remove-comment-from-file-at-point ()
  ;; TODO add to multiple files?
  (interactive)
  (let* ((file (dired-get-filename))
         (data (filemeta-for-file file))
         (comments (filemeta-comments data))
         (c-to-remove (ivy-read "Select a comment to remove: " comments)))
    (filemeta-remove-comment-from-file file c-to-remove)))
