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

;;;;;; functions ;;;;;;

;; util
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

(defun add-filemeta-at-point ()
  (read-file-to-db) ;; TODO
  (push (make-filemeta
         :path (filemeta-dired-get-file-at-point)
         :comments (read-comment) ;; TODO learn indirect buffer
         :tags (read-tags)        ;; TODO learn ivy-read for this.. can i select multiple?
         :hists nil)
        *filemeta-db*)
  (write-db-to-file) ;; TODO
)
