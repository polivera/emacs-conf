;;; use-php.el --- PHP specific configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration for programming in php.

;;; Code:

(use-package php-mode
  :ensure t
  :demand t)

(use-package php-ts-mode
  :ensure (:host github :repo "emacs-php/php-ts-mode")
  :demand t)

(use-package emacs
  :ensure nil
  :hook (
	 (php-ts-mode . lsp-mode)
	 )
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))

  ;; TODO: Create a folder for intelephense cache and set the variable
  ;; lsp-intelephense-storage-path to the autogen folder.

  (add-hook 'go-ts-mode-hook (lambda()
			       (setq tab-width 4)
			       (setq php-ts-mode-indent-offset 4)
			       (setq truncate-lines t))))

(provide 'use-php)
;;; use-php.el ends here.
