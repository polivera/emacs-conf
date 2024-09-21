;;; use-embark.el -*- lexical-binding: t; -*-
;;; Commentary: 
;;; Code:
(use-package embark
  :ensure t
  :defer t
  :general
  (xap/quick-leader
    "," '(embark-act :which-key "Embark Act")
  )
  (xap/leader-key
    "e" '(nil :which-key "Embark")
    "ee" '(embark-export :which-key "Export")
    "ec" '(embark-collect :which-key "Collect")
    "ea" '(embark-act :which-key "Act")
  ))

(use-package embark-consult
  :ensure t
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'use-embark)

