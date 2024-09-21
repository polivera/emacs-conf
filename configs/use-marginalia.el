;;; use-marginalia.el -*- lexical-binding: t; -*-
;;; Commentary: This package adds marginalia to the minibuffer completions.
;;; Code:
(use-package marginalia
  :ensure t
  :defer t
  :init
  (marginalia-mode))

(provide 'use-marginalia)
;;; end use-marginalia.el

