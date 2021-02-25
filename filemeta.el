;;;;;; structures ;;;;;;

(defstruct filemeta
  path comments tags hists) ;; TODO add time for comments

(setq filemeta-eg1
      (make-filemeta
       :path "~/test-image/1.jpg"
       :comments '("I love this photo.")
       :tags '(college math career)
       :hists nil))

(setq filemeta-eg2
      (make-filemeta
       :path "~/test-image/2.jpg"
       :comments '("I dislike this photo.")
       :tags '(college math career)
       :hists nil))

(setq *filemeta-db* nil)
(setq *filemeta-db-path* "/tmp/filemeta.db")
(push filemeta-eg1 *filemeta-db*)
(push filemeta-eg2 *filemeta-db*)
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
(defun filemeta-match-with-p ())
(defun filemeta-filter-with ())

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

;; hash
(defun filemeta-update-history ())
(defun filemeta-show-history ())

;; db
(defun filemeta-add (filemeta)
  (cl-pushnew filemeta *filemeta-db*))
;(defun filemeta-add-interactive ())
(defun filemeta-remove (filemeta)
  (setf *filemeta-db*
        (remove filemeta *filemeta-db*)))
;(defun filemeta-remove-interactive ())

;; I/O
;; TODO eventually i want to store each filemeta in its own file by hashing
(defun filemeta-write-db ()
  "Overwrite *FILEMETA-DB-PATH* with a READable form of
  *FILEMETA-DB*."
  (write-region (prin1-to-string *filemeta-db*)
                nil *filemeta-db-path*))
;(defun filemeta-safely-write-db (path))

(defun filemeta-read-db ()
  "Read the PATH and expect a list of FILEMETAs."
  (setq *filemeta-db*
        (read (f-read-text *filemeta-db-path* 'utf-8))))
;(defun filemeta-safely-read-db (path))

;; dired
(defun filemeta-dired-do-at-point (operation)
  "Extract filepath at point, and destructively apply OPERATION
  on all filemetas in *FILEMETA-DB*.")

(defun filemeta-dired-get-file-at-point ()
  "Get the file at point."
  (let ((file (if (derived-mode-p 'dired-mode)
                  (dired-get-filename)
                (buffer-file-name))))
    file))

(defun filemeta-dired-get-files-at-point ()
  "Get the marked files at point."
  (let ((files (if (derived-mode-p 'dired-mode)
                   (dired-get-marked-files)
                 (buffer-file-name))))
    files))


;;; TODO FIXME working on interactive add meta
;;; (setq filemeta-eg1
;;;       (make-filemeta
;;;        :path "~/test-image/1.jpg"
;;;        :comments '("I love this photo.")
;;;        :tags '(college math career)
;;;        :hists nil))

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
    (write-region (prin1-to-string data) nil data-file)
    ))

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
  (interactive)
  (print (filemeta-for-file (dired-get-filename))))

(defun filemeta-add-tag-to-file-at-point ()
  (interactive)
  (let ((tags-string (ivy-read "Enter tags: " nil)) ;; TODO read candidates from a tag db
        (tags (mapcar #'intern (split-string (s-collapse-whitespace tags-string))))) ;; tokenize the tags
    (loop for tag in tags do
          (filemeta-add-tag-to-file (dired-get-filename) tag))))

(defun filemeta-remove-tag-from-file-at-point ()
  (interactive)
  (let* ((file (dired-get-filename))
         (data (filemeta-for-file file))
         (tags (filemeta-tags data))
         (tags-string (ivy-read "Select tag to remove: " tags))
         (tags-to-remove (mapcar #'intern (split-string (s-collapse-whitespace tags-string)))))
    (loop for tag-to-remove in tags-to-remove do
          (filemeta-remove-tag-from-file file tag-to-remove))))

(defun filemeta-add-comment-to-file-at-point () )
(defun filemeta-remove-comment-from-file-at-point () "TODO") ; use ivy to select which comment to remove
