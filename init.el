;;; Init.el --- Emacs configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;; Starting point of Emacs configuration file.

;;; Code:

;; base config
(require 'use-elpaca)
(require 'use-base-config)

;; Theme and layout
(require 'use-themes)
(require 'use-modeline)

;; keybindings
(require 'use-general)
(require 'use-evil)
(require 'use-which-key)

;; minibuffer
(require 'use-vertico)
(require 'use-orderless)
(require 'use-consult)
(require 'use-embark)
(require 'use-marginalia)

;; Org Stuff
(require 'use-org)
(require 'use-org-roam)

;; project stuff
(require 'use-projectile)
(require 'use-magit)
(require 'use-dired)
(require 'use-treemacs)
(require 'use-hl-todo)

;; tree-sitter
(require 'use-treesit)

;; development
(require 'use-lsp)
(require 'use-company)
(require 'use-flycheck)
(require 'use-yasnippet)

;; development - language specific.
(require 'use-elisp)
(require 'use-go)
(require 'use-php)
(require 'use-c)
(require 'use-makefile)
(require 'use-protobuf)

;; Enable debugging information
;; (setq debug-on-error t)
;; (setq warning-minimum-log-level :debug)

;; Custom packages
(use-package emacs
  :ensure nil
  :config
  (require 'xap-utils)

  ;; Handle PATH on emacs
  (xaputils/update-path)
  (setq exec-path (xaputils/get-path-list))

  ;; TODO: Move this to another file at some point
  (require 'ansi-color)
  (defun xapfunc/set-ansi-colors-on-compile-buffer ()
    "Apply ANSI color codes to the current `compilation-filter' output."
    (ansi-color-apply-on-region compilation-filter-start (point)))
  (add-hook 'compilation-filter-hook 'xapfunc/set-ansi-colors-on-compile-buffer)
  ;; end

  (require 'testonga-go)
  (require 'testonga-php)
  (require 'save-exec)
  (require 'env-loader))

;;; init.el ends here.
