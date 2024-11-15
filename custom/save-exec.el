;;; save-exec.el --- Execute a commmand after file save -*- lexical-binding: t; -*-

;;; Commentary:

;; Create a package that execute a command each time the file is saved.
;; It should be able to:
;;  - Run a command or a list of commands on save
;;  - Have a configurable map of modes->commands or file-regexp->command
;;  - Disable or enable de command execution

;;; Code:

(require 'xap-utils)

(defgroup save-exec-mode-group nil
  "Save Exec group."
  :prefix "save-exec-mode-"
  :group 'editing)

;; --------------------------------------------------------------------------
;; Command list with its setters and getters
(defcustom save-exec-mode-commands
  '(("go" . "gosimports -w ###file###")
    ("json" . "prettier -w ###file###")
    ("php" . "php-cs-fixer fix ###file### --using-cache=no --rules=@PSR12,@PSR1,@PSR2")
    ("c" . "clang-format -i --style=LLVM ###file###")
    ("h" . "clang-format -i --style=LLVM ###file###")
    ("sh" . "shfmt -l -w ###file###")
    )
  "An assoc list with file extension and command."
  :type 'list
  :group 'save-exec)

(defun save-exec-mode-get-command (ext)
  "Get the command associated with the given EXT."
  (cdr (assoc ext save-exec-mode-commands)))

(defun save-exec-mode-set-command (ext command)
  "Insert or update EXT with the given COMMAND."
  (if (assoc ext save-exec-mode-commands)
      (setcdr (assoc ext save-exec-mode-commands) command)
    (push (cons ext command) save-exec-mode-commands)))
;; --------------------------------------------------------------------------
    
(defun save-exec-mode--execute()
  "Execute the provided command after save."
  (let* ((f-extension (file-name-extension (buffer-file-name)))
	 ;; Get the current buffer
	 (cmd (save-exec-mode-get-command f-extension)))
    (when cmd
      (setq cmd (string-replace "###file###" (buffer-file-name) cmd))
      (when cmd
	(xaputils/run-background-command
	 cmd
	 (lambda()
	   (revert-buffer nil t t)))))))

(define-minor-mode save-exec-mode
  "Automatically execute provided command after file save.
See `save-exec-mode-commands` to configure."
  :lighter " SE"
  :group 'save-exec-mode-group
  (if save-exec-mode
      (add-hook 'after-save-hook 'save-exec-mode--execute)
    (remove-hook 'after-save-hook 'save-exec-mode--execute)))


;; --------------------------------------------------------------------------
;; Enable mode
(use-package emacs
  :ensure nil
  :config
  (add-hook 'go-ts-mode-hook #'save-exec-mode)
  (add-hook 'php-ts-mode-hook #'save-exec-mode)
  (add-hook 'c-ts-mode-hook #'save-exec-mode)
  (add-hook 'json-ts-mode-hook #'save-exec-mode))

(provide 'save-exec)
;;; save-exec.el ends here.
