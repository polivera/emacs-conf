;;; use-flycheck.el --- Flycheck configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal configuration for flycheck package

;;; Code:

(use-package flycheck
  :ensure t
  :demand t
  :init
  (global-flycheck-mode))


(provide 'use-flycheck)
;;; use-flycheck.el ends here
