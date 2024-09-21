;;; use-projectile.el -*- lexical-binding: t; -*-
;;; Commentary: This package provides orderless completion that divides pattern in to space-separated components
;;; Code:
(use-package projectile
  :ensure t
  :demand t
  :general
  (xap/leader-key
    "p"   '(nil :which-key "Projectile")
    "p s" '(projectile-switch-project :which-key "Switch Project")
    "p a" '(projectile-add-known-project :which-key "Add Known Project")
    "p t" '(projectile-run-vterm-other-window :which-key "Run vterm")
  )
  :init
  (setq projectile-cache-file xapvar/projectile-cache-file)
  (setq projectile-known-projects-file xapvar/projectile-bookmarks-file)
  (projectile-mode +1)
)

(provide 'use-projectile)
;;; end use-projectile.el

