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


(defun xaputils/exec-command-no-color (command buffer-name &optional command-title command-name)
  "Execute a shell COMMAND and display the output in a temporary buffer.
The temp buffer will be named BUFFER-NAME and can be referenced in later calls.
COMMAND-TITLE will be the first line of the new buffer if set.
COMMAND-NAME will be passed to \"start-process-shell-command\".
The result buffer is shown WITHOUT ansi colors."
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


(defun xaputils/exec-command (command buffer-name &optional command-title command-name)
  "Execute a shell COMMAND and display the output in a temporary buffer.
The temp buffer will be named BUFFER-NAME and can be referenced in later calls.
COMMAND-TITLE will be the first line of the new buffer if set.
COMMAND-NAME will be passed to `start-process-shell-command`."
  (require 'ansi-color)
  (setq command-title (or command-title (format "Running command: %s" command)))
  (setq command-name (or command-name "mycommand"))
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (read-only-mode 0)
      (erase-buffer)
      ;;(insert (concat command-title "\n"))
      (read-only-mode 1)
      (special-mode))
    (pop-to-buffer buffer)

    (let ((proc (start-process-shell-command command-name buffer command)))
      (set-process-filter
       proc
       (lambda (proc output)
         (with-current-buffer (process-buffer proc)
           (let ((inhibit-read-only t))
             ;; Insert the command output
             (insert output)
             ;; Apply ANSI color translation
             (ansi-color-apply-on-region (point-min) (point-max)))))))
    ))


(defun xaputils/run-background-command (command callback &optional process-name)
  "Run COMMAND and execute CALLBACK after finished.
Optionally you can use PROCESS-NAME to name the process"
  (setq process-name (or process-name "xaputils-back-cmd"))
  (message command)
  (let ((process (start-process-shell-command process-name "*Messages*" command)))
    (set-process-sentinel
     process
     (lambda (_ event)
       (when (string= event "finished\n")
	 (funcall callback))))))


(defun xaputils/get-path-list()
  "The all $PATH entries in a list."
  (split-string (getenv "PATH") ":"))


(defun xaputils/update-path()
  "Update Emacs path value with zsh path value."
  (let ((shell-cmd "/usr/bin/zsh"))
    (when xapconst/is-macos
      (setq shell-cmd "/bin/zsh"))
    (let ((path-from-shell (shell-command-to-string (concat shell-cmd " -i -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell))))
  
(provide 'xap-utils)
;;; xap-utils.el ends here
