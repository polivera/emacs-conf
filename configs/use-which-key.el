;;; use-which-key.el --- Which key configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package which-key
  :ensure t
  :demand t
  :after (general)
  :general
  (xap/leader-key
    "?" 'which-key-show-top-level)
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

(provide 'use-which-key)
;;; use-which-key.el ends here.
