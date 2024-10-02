;;; use-go.el --- Golang specific configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration for programming in golang.

;;; Code:
(use-package emacs
  :ensure nil
  :hook (
	 (go-ts-mode . lsp-mode)
	 (go-mod-ts-mode . lsp-mode)
	 )
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))

  (add-hook 'go-ts-mode-hook (lambda()
			       (setq tab-width 4)
			       (setq go-ts-mode-indent-offset 4)
			       (setq truncate-lines t)))
			       
)


(provide 'use-go)
;;; use-go.el ends here.
