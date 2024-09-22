;;; use-flycheck.el --- Flycheck configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal configuration for flycheck package

;;; Code:

(use-package flycheck
  :ensure (:wait t)
  :demand t
  :init
  ;; This makes flycheck to inherit the current path for elisp
  ;; instead of using the default path without user added ones.
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode)
)


(provide 'use-flycheck)
;;; use-flycheck.el ends here.
