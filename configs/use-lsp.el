;;; use-lsp.el --- LSP Configuration for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure LSP client for Emacs

;;; Code:

(require 'custom-vars)

(use-package lsp-mode
  :ensure t
  :demand t
  :commands
  (lsp lsp-deferred)
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-session-file xapconst/lsp-session-file)
  (setq lsp-file-watch-threshold 100000)
  :general
  (xap/leader-key
    "l"   '(nil :which-key "LSP")
    "l r" '(lsp-find-references :which-key "Find reference")
    "l i" '(lsp-find-implementation :which-key "Find implementation")
    "l d" '(lsp-find-definition :which-key "Find definition")
    "l c" '(lsp-find-declaration :which-key "Find declaration")
    "l n" '(lsp-rename :which-key "Rename")
    "l a" '(lsp-execute-code-action :which-key "Code action")
    "l o" '(lsp-organize-imports :which-key "Organize imports")
    "l k" '(lsp-ui-doc-show :which-key "Show documentation")
  )
)

(use-package lsp-ui
  :after (lsp-mode)
  :ensure t
  :demand t
  :hook
  (prog-mode . lsp-ui-sideline-mode)
)


(provide 'use-lsp)
;;; use-lsp.el ends here.
