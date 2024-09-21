;;; use-elisp.el --- Emacs lisp configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Tools for develop on elisp

;;; Code:
(use-package emacs
  :ensure nil
  :config
  (global-set-key (kbd "C-c C-c") 'eval-buffer)    ;; Evaluate the whole buffer
  (global-set-key (kbd "C-c C-k") 'eval-defun)     ;; Evaluate the current function
  (global-set-key (kbd "C-c C-e") 'eval-last-sexp) ;; Evaluate the last expression
)

(provide 'use-elisp)
;;; use-elisp.el ends here
