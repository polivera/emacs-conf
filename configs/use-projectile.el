;;; use-projectile.el --- Projectile configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides orderless completion that divides pattern in to space-separated components

;;; Code:

;; Include custom vars.
(require 'custom-vars)

(use-package projectile
  :ensure t
  :demand t
  :general
  (xap/leader-key
    "p"   '(nil :which-key "Projectile")
    "p s" '(projectile-switch-project :which-key "Switch Project")
    "p a" '(projectile-add-known-project :which-key "Add Known Project")
    "p t" '(projectile-run-vterm-other-window :which-key "Run vterm")
    "p c" '(projectile-compile-project :which-key "Compile")
    "p r" '(projectile-run-project :which-key "Run")
    "p x" '(projectile-kill-buffers :which-key "Kill")
    "p o" '(xapfunc/projectile-todo-search :which-key "Search TODOS")
  )
  :init
  (setq projectile-cache-file xapconst/projectile-cache-file)
  (setq projectile-known-projects-file xapconst/projectile-bookmarks-file)
  (projectile-mode +1)
  :config
  ;; Use Projectile to jump to TODOs in the project
  (defun xapfunc/projectile-todo-search ()
    "Search for TODO comments in the current Projectile project."
    (interactive)
    (consult-ripgrep (projectile-project-root) "TODO\\|FIXME\\|NOTE\\|HACK\\|REVIEW\\|OPTIMIZE"))
)

(use-package vterm
  :ensure t
  :defer t)

(provide 'use-projectile)
;;; use-projectile.el ends here.

