;;; use-elisp.el --- Emacs lisp configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Tools for develop on elisp

;;; Code:
(use-package emacs
  :ensure nil
  :general
  (xap/leader-key
    "i" '(nil :which-key "elisp eval")
    "i b" '(eval-buffer :which-key "Buffer")
    "i k" '(eval-defun :which-key "Function")
    "i l" '(eval-last-sexp :which-key "Last Expression"))
)


(provide 'use-elisp)
;;; use-elisp.el ends here
