;;; use-modeline.el --- modeline configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal configuration for modeline related stuff

;;; Code:

;; Include custom vars
(require 'custom-vars)

(use-package nerd-icons
  :ensure t
  :demand t
  :custom
  (nerd-icons-font-family xapconst/default-font-family))

(use-package doom-modeline
  :ensure t
  :demand t
  :after (nerd-icons)
  :init
  (doom-modeline-mode 1)
  :config
  ;; Update branch name
  (defvar auto-revert-check-vc-info t)

  (defvar doom-modeline-height 25)
  (defvar doom-modeline-time-analogue-clock nil)
  (defvar doom-modeline-time-icon nil)
  (defvar doom-modeline-vcs-icon nil)
  (defvar doom-modeline-vcs-max-length 20)
  (defvar doom-modeline-position-column-line-format '("%l:%c"))
  (defvar doom-modeline-buffer-file-name-style 'file-name)
  (defvar doom-modeline-buffer-encoding nil)
)


(provide 'use-modeline)
;;; use-modeline.el ends here.
