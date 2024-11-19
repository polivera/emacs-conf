;;; use-yasnippet.el --- A Snippet package-*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package yasnippet
  :ensure t
  :demand t
  :config
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  (yas-global-mode 1))


(provide 'use-yasnippet)
;;; use-yasnippet.el ends here.
