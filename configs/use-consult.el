;;; use-consult.el --- Consult configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure consult for Emacs.
;; Package homepage: https://github.com/minad/consult

;;; Code:
(use-package consult
  :ensure t
  :defer t
  :after (:all general which-key)
  :general
  (xap/leader-key
    "f" '(nil :which-key "Find") 
    "ff" '(consult-fd :which-key "Files")
    "fj" '(consult-ripgrep :which-key "Text in files")
    "fb" '(consult-bookmark :which-key "Bookmarks")
    "fo" '(consult-outline :which-key "Org Outline")
    "fb" '(consult-buffer :which-key "Buffers"))
  (xap/quick-leader
    "b" '(consult-buffer :which-key "List buffers")
    "f" '(consult-fd :which-key "Find files")
    "j" '(consult-ripgrep :which-key "Find text")
    "o" '(consult-outline :which-key "Org outline"))
  :init
  ;; Improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  )

(provide 'use-consult)
;;; use-consult.el ends here.
