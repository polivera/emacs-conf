;;; use-general.el -*- lexical-binding: t; -*-
;;; Commentary: General key binder plugin
;;; Code:
(use-package general
  :ensure (:wait t) 
  :demand t
  :config
  (general-create-definer xap/leader-key
    :prefix "C-a"
    :keymaps 'override)
  (general-create-definer xap/quick-leader
    :prefix "C-s"
    :keymaps 'override)

  ;; Define some keys
  (xap/quick-leader
    "n" 'find-file
    "h" 'next-buffer
    "l" 'previous-buffer
    "s" 'save-buffer
    "q" 'revert-buffer
    "/" 'comment-or-uncomment-region)
)


(provide 'use-general)
;;; end use-general.el
