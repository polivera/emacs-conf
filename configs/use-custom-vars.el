;;; use-custom-vars.el  -*- lexical-binding: t; -*-
;;; Commentary: Set custom variables to use thourout emacs configuration
;;; Code:

(use-package emacs
    :ensure nil
    :init

    ;;; CheckOS
    (defvar xapvar/is-linux (eq system-type 'gnu/linux) "t if it is a linux system, nil otherwise")
    (defvar xapvar/is-macos (eq system-type 'darwin) "t if it is a mac os system, nil otherwise")
    (defvar xapvar/is-window (eq system-type 'windows-nt) "t if it is a windows system, nil otherwise")

    ;;; Custom fonts
    (defvar xapvar/default-font-family "IosevkaTerm Nerd Font" "Default font family")
    (defvar xapvar/default-font-size 130 "Default Emacs font size")
    (when xapvar/is-linux
      (setq xapvar/default-font-size 125))
    (when xapvar/is-macos
      (setq xapvar/default-font-size 145))

    (defvar xapvar/transient-history (concat xapvar/autogen-folder-path "transient-history.el") "Set path to transient-history-file")
    (defvar xapvar/recentf-autogen-file (concat xapvar/autogen-folder-path "recentf") "Recentf db file path")
    (defvar xapvar/savehist-autogen-file (concat xapvar/autogen-folder-path "savehist") "Savehist files path")
    (defvar xapvar/places-autogen-file (concat xapvar/autogen-folder-path "places") "Places files path")
    (defvar xapvar/auto-saves-path (concat xapvar/autogen-folder-path "auto-saves/") "Path to folder that contains autosave data")
    (defvar xapvar/file-name-handler-alist file-name-handler-alist "Default value of file-name-handler-alist")

    ;;; Projectile
    (defvar xapvar/projectile-bookmarks-file (concat xapvar/autogen-folder-path "projectile-bookmarks.eld") "Path to projectile known projects file")
    (defvar xapvar/projectile-cache-file (concat xapvar/autogen-folder-path "projectile.cache") "Path to projectile cache file")
)


(provide 'use-custom-vars)
;;; end use-custom-vars.el
