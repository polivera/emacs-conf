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
  )
  :init
  (setq projectile-cache-file xapconst/projectile-cache-file)
  (setq projectile-known-projects-file xapconst/projectile-bookmarks-file)
  (projectile-mode +1)
)

(provide 'use-projectile)
;;; use-projectile.el ends here.

