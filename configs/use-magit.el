;;; use-magit.el --- Magit configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;Magit is a complete text-based user interface to Git.

;;; Code:

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :defer t
  :general
  (xap/quick-leader
    "g" 'magit-status)
  (xap/leader-key
    "g"   '(nil :which-key "Magit")
    "g s" '(magit-status :which-key "Status")
    "g f" '(magit-file-dispatch :which-key "File")
    "g l" '(magit-log :which-key "Log"))
  :config
  (setq magit-log-arguments '("--graph" "--decorate" "--color"))
  (setq magit-define-global-key-bindings nil)
  ;; (setq magit-section-visibility-indicator '("⮧"))
)

(use-package diff-hl
  :after (magit)
  :ensure t
  :defer t
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode)
  )


(provide 'use-magit)
;;; use-magit.el ends here.
