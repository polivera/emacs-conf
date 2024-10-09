;;; use-c.el --- C / C++ specific configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration for programming in C or C++.

;;; Code:

(require 'treesit)

(use-package emacs
  :ensure nil
  :hook (
	 (c-ts-mode . lsp-mode)
	 )
  :config
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))

  (add-hook 'c-ts-mode-hook (lambda()
			      (setq indent-tabs-mode nil)
			      (setq tab-width 2)
			      (setq c-ts-mode-indent-offset 2)
			      (setq truncate-lines t))))


(provide 'use-c)
;;; use-c.el ends here.
