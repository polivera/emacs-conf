;;; use-orderless.el -*- lexical-binding: t; -*-
;;; Commentary: This package provides orderless completion that divides pattern in to space-separated components
;;; Code:
(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(provide 'use-orderless)
;;; end use-orderless.el
