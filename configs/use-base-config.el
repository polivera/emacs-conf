;;; use-base-config.el --- Base configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This configuration must be called before anything else but after
;; custom variables/constants definition

;;; Code:

;; Include cusotm variables
(require 'custom-vars)

(use-package emacs
  :ensure nil
  :init

  ;; Clean up the mode line
  (display-time-mode -1)
  (setq column-number-mode t)

  ;; Less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  
  (setq load-prefer-newer t)

  ;; Set modeline options
  (display-time-mode 1)
  (setq column-number-mode t)

  ;; always allow 'y' instead of 'yes'.
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Start maximize
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Font configuration
  (when (find-font (font-spec :name xapconst/default-font-family))
    (set-face-attribute 'default nil :font xapconst/default-font-family :height xapvar/default-font-size)
    (set-face-attribute 'fixed-pitch nil :font xapconst/default-font-family)
    (set-face-attribute 'variable-pitch nil :font xapconst/default-font-family)
    )

  ;; Autogenerate files
  (setq transient-history-file xapconst/transient-history) ;; Set transient file to junk
  (setq make-backup-files nil) ;; Disable backup files
  (setq custom-file (make-temp-file "emacs-custom")) ;; Set custom config to a temporary file
  (setq recentf-save-file xapconst/recentf-autogen-file) ;; recentf
  (setq savehist-file xapconst/savehist-autogen-file) ;; savehist
  (setq save-place-file xapconst/places-autogen-file) ;; places
  (setq auto-save-list-file-prefix xapconst/auto-saves-path) ;; Auto save session, files and locks
  (setq auto-save-file-name-transforms `((".*" ,xapconst/auto-saves-path t))) ;; Set lock files path
  (setq lock-file-name-transforms `((".*" ,xapconst/auto-saves-path t)))

  ;; Exclusive macOS configs
  (when xapconst/is-macos
    (setq mac-command-modifier 'meta)     ; command as super
    (setq mac-option-modifier 'super)     ; alt as meta
    (setq mac-control-modifier 'control))
  
  ;; Set zoom in and zoom out
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "C-0") #'(lambda() (interactive) (text-scale-set 0)))

  ;; Display line number
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
)

(provide 'use-base-config)
