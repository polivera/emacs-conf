;;; init.el --- Emacs configuration file -*- lexical-binding: t; -*-

;;; Commentary:

;; Starting point of Emacs configuration file.

;;; Code:

;; This set folders where the configuration is located
;;     configs: repository package configuration files
;;     custom: my custom packages (with configuration)
(mapc
    (lambda (string)
    (add-to-list 'load-path (locate-user-emacs-file string)))
    '("./configs" "./custom"))

;; base config
(require 'use-elpaca)
(require 'use-custom-vars)
(require 'use-base-config)

;; Theme and layout
(require 'use-themes)
(require 'use-modeline)

;; keybindings
(require 'use-evil)
(require 'use-general)
(require 'use-which-key)

;; minibuffer
(require 'use-vertico)
(require 'use-orderless)
(require 'use-consult)
(require 'use-embark)
(require 'use-marginalia)

;; project stuff
(require 'use-projectile)
(require 'use-magit)

;; tree-sitter
(require 'use-treesit)

;; development
(require 'use-company)
(require 'use-flycheck)

;; development - language specific
(require 'use-elisp)

;;; init.el ends here
