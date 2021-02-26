;;; filemeta-health.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(defun filemeta-health-check-data-file (data-file)
  "Check if DATA-FILE contains a healthy filemeta."
  (filemeta-p
   (filemeta-filemeta-from-data-file data-file)))

(defun filemeta-health-check-file (file)
  "Check if the data-file of FILE contains a healthy filemeta."
  (filemeta-health-check-data-file
   (filemeta-data-for-file file)))

(defun filemeta-ls-unhealthy-data-files ()
  "Return the list of all unhealthy data files under
  *FILEMETA-ROOT-DIR*."
  (loop for data-file in (filemeta-ls-data-files)
        unless (filemeta-health-check-data-file data-file)
        collect data-file))

(defun filemeta-health-check-db ()
  "Return T if all data files under *FILEMETA-ROOT-DIR* are
  healthy."
  (let ((patients (filemeta-ls-unhealthy-data-files)))
    (if (eq nil patients)
        (message "It's healthy.")
      (message "List of unhealthy data files:\n%s."
               patients))))

(provide 'filemeta-health)
