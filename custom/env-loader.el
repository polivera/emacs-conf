;;; env-loader.el --- Load environment file from the project root -*- lexical-binding: t; -*-

;;; Commentary:

;; Create a package that loads an env file from the root of the project.
;; It should be able to:
;;   - Load a file of environment variables.
;;   - Edit an environment variable for the session.
;;   - Disable an environment variable for the session.
;;   - Update an environment variable on the file.
;;   - Delete an environment variable from the file.

;;; Code:

(require 'projectile)

(defgroup env-loader-group nil
  "Save Exec group."
  :prefix "env-loader-"
  :group 'editing)


(defcustom env-loader-file-path
  ".xapenv"
  "File to load the environment vars.
It should be relative to the project root."
  :type 'string
  :group 'env-loader-group)
  

(defun env-loader--load-file()
  "Load the content of the env file.
The env file should be located on the path from `env-loader-file-path`.
The value from `env-loader-file-path` will be concatenated to project root path."
  (message "Loading environment file: %s" (concat (projectile-project-root) env-loader-file-path))
  (let ((file-path (concat (projectile-project-root) env-loader-file-path)))
    (when (file-exists-p file-path)
      (with-temp-buffer
	(insert-file-contents file-path)
	(buffer-string)))))


(defun env-loader--split-var-line (var-line)
  "Split the environment variable VAR-LINE into a name / value construct."
  (setq var-line (string-replace "\"" "" var-line))
  (message var-line)
  (let ((pos (string-match (regexp-quote "=") var-line)))
    (if pos
	(let ((part-one (substring var-line 0 pos))
	      (part-two (substring var-line (+ pos 1))))
	  (list part-one part-two)))))


(defun env-loader--load()
  "Load environment variables from specified file.
See `env-loader-file-path`."
  (let* ((env-content (env-loader--load-file))
	(var-list (split-string env-content "\n")))
    (dolist (env-var var-list)
      (when (and (> (length env-var) 0)
		 (let ((first-char (aref env-var 0)))
		   (char-uppercase-p first-char)))
	(let* ((var-parts (env-loader--split-var-line env-var)))
	  (when var-parts
	    (let ((var-name (car var-parts))
		  (var-value (nth 1 var-parts)))
	      (message var-name)
	      (setenv var-name var-value))))))))
    

(defun env-loader-load()
  "Interactive method to load environment variables."
  (interactive)
  (env-loader--load))


;; Load the env file if exist on project change
(add-hook 'projectile-after-switch-project-hook 'env-loader--load)

(provide 'env-loader)
;;; env-loader.el ends here.
