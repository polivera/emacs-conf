;;; use-company.el -*- lexical-binding: t; -*-
;;; Commentary: 
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
;;; end use-company.el
