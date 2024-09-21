;;; use-vertico.el -*- lexical-binding: t; -*-
;;; Commentary: 
;;; Code:

(use-package vertico
  :ensure t
  :defer t
  :general
  (:keymaps 'vertico-map
    "C-j" #'vertico-next
    "C-k" #'vertico-previous)
  :init
  (vertico-mode))


;; Savehist
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))


;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  (setq enable-recursive-minibuffers t)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))


(provide 'use-vertico)
