;;; xap-utils.el --- Utilities to code package -*- lexical-binding: t; -*-

;;; Commentary:

;; Various utility methods that I find useful not to rewrite again
;; when I am working on a package

;;; Code:


;; Declare some functions to make flycheck happy
(declare-function treesit-buffer-root-node "treesit")
(declare-function treesit-query-capture "treesit")
(declare-function treesit-node-text "treesit")
(defun xaputils/parse-query (query capture-list)
  "Run the QUERY and return a list of captured elements that match CAPTURE-LIST."
  (let* ((root-node (treesit-buffer-root-node))
	 (matches (treesit-query-capture root-node query))
	 (captures ()))
    (dolist (match matches)
      (let ((capture-var-name (car match))
	    (capture-text (treesit-node-text (cdr match) 'no-properties)))
	(when (member-ignore-case (format "%s" capture-var-name) capture-list)
	  (cl-pushnew capture-text captures :test #'string=))))
    captures))


(defun xaputils/exec-command (command buffer-name &optional command-title command-name)
  "Execute a shell COMMAND and display the output in a temporary buffer.
The temp buffer will be named BUFFER-NAME and can be referenced in later calls.
COMMAND-TITLE will be the first line of the new buffer if set.
COMMAND-NAME will be passed to \"start-process-shell-command\"."
  (setq command-title (or command-title (format "Running command: %s" command)))
  (setq command-name (or command-name "mycommand"))
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (read-only-mode 0)
      (erase-buffer)
      (insert (concat command-title "\n"))
      (read-only-mode 1)
      (special-mode))
    (start-process-shell-command command-name buffer command)
    (pop-to-buffer buffer)))

(provide 'xap-utils)
;;; xap-utils.el ends here
