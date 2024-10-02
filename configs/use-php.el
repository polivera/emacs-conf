;;; use-php.el --- PHP specific configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration for programming in php.

;;; Code:

(use-package emacs
  :ensure nil
  :hook (
	 (php-ts-mode . lsp-mode)
	 )
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))

  (add-hook 'go-ts-mode-hook (lambda()
			       (setq tab-width 4)
			       ;; (setq go-ts-mode-indent-offset 4)
			       (setq truncate-lines t)))
			       
)


(provide 'use-php)
;;; use-php.el ends here.
