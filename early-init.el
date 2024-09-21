;;; early-init.el --- Early configuration file -*- lexical-binding: t; -*-
;;; Commentary:

;; Pre-init confiugration

;;; Code:

;; Define custom variables
(defvar xapvar/autogen-folder-path (concat user-emacs-directory "autogen/") "Path to auto generated files.")
(defvar xapvar/eln-cache-path (concat xapvar/autogen-folder-path "eln-cache/") "Path to eln cache direcory.")
(defvar xapvar/gc-cons-threshold 100000000 "Default value of \"gc-cons-threshold\".")
(defvar xapvar/gc-cons-percentage gc-cons-percentage "Default value of \"gc-cons-percentage\".")
(defvar xapvar/vc-handled-backends vc-handled-backends "Default value of \"vc-handled-backends\".")
(defvar xapvar/read-process-output (* 1024 3072) "Default value for \"read-process-output-max\" (3M).")
(defvar xapvar/initial-background-color "#25272f" "Initial background color (before apply theme).")
(defvar xapvar/initial-foreground-color "#B3B9C4" "Initial foreground color (before apply theme).")

;; Garbage collector
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
  (lambda()
    (setq gc-cons-threshold xapvar/gc-cons-threshold
          gc-cons-percentage xapvar/gc-cons-percentage
          file-name-halder-alist xapvar/file-name-handler-alist
          vc-handled-backends xapvar/vc-handled-backends
          read-process-output-max xapvar/read-process-output)))


;; Create foldder for auto-generated files if it does not exist
(unless (file-directory-p xapvar/autogen-folder-path)
  (make-directory xapvar/autogen-folder-path) (make-directory xapvar/eln-cache-path))

(when (native-comp-available-p)
  ;; Remove the original eln-cache.
  (setq native-comp-eln-load-path (cdr native-comp-eln-load-path))
  ;; Add the new eln-cache.
  (push xapvar/eln-cache-path native-comp-eln-load-path))


;; Layout configuration ------------------------------------------------------------------------------------------------
(set-face-background 'default xapvar/initial-background-color)
(set-face-foreground 'default xapvar/initial-foreground-color)
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
