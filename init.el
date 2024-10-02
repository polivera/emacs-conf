;;; init.el --- Emacs configuration file -*- lexical-binding: t; -*-

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
(require 'use-evil)
(require 'use-general)
(require 'use-which-key)

;; minibuffer
(require 'use-vertico)
(require 'use-orderless)
(require 'use-consult)
(require 'use-embark)
(require 'use-marginalia)

;; Org Stuff
(require 'use-org)

;; project stuff
(require 'use-projectile)
(require 'use-magit)

;; tree-sitter
(require 'use-treesit)

;; development
(require 'use-lsp)
(require 'use-company)
(require 'use-flycheck)

;; development - language specific.
(require 'use-elisp)
(require 'use-go)
;; (require 'use-php)
;; (require 'use-c)


;; Custom packages
(use-package emacs
  :ensure nil
  :config
  ;; Load PATH env var into emacs env.
  (when xapconst/is-linux
    (setenv "TERM" "vterm") ;; TERM is set so I don't have the dumb term error.
    (let ((path-from-shell (shell-command-to-string "/usr/bin/zsh -i -c 'echo $PATH'")))
      (setenv "PATH" path-from-shell)))

  (require 'testonga-go)
  (require 'save-exec)
  (require 'env-loader))

;;; init.el ends here.
