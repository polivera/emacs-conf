;;; use-company.el --- Company completion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Company is a text and code completion framework for Emacs.
;; The name stands for "complete anything".
;; Homepage: https://company-mode.github.io/

;;; Code:

(use-package company
  :ensure t
  :defer t
  :hook (
	 (prog-mode . company-mode)
	 (text-mode . company-mode)
	 )
  :config
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-complete-selection))


(provide 'use-company)
;;; use-company.el ends here.
