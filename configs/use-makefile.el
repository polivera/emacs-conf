;;; use-makefile.el --- Makefile specific configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration for makefiles

;;; Code:

(use-package emacs
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-mode))

  (add-hook 'makefile-mode-hook (lambda()
				  (setq tab-width 4)
				  (setq truncate-lines t))))


(provide 'use-makefile)
;;; use-makefile.el ends here.
