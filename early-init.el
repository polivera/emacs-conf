;;; early-init.el --- Early configuration file -*- lexical-binding: t; -*-
;;; Commentary:

;; Pre-init confiugration

;;; Code:

;; This set folders where the configuration is located
;;     configs: repository package configuration files
;;     custom: my custom packages (with configuration)
(mapc
    (lambda (string)
        (add-to-list 'load-path (locate-user-emacs-file string)))
        '("./configs" "./custom"))

;; Require custom vars to be used here
(require 'custom-vars)

;; Define custom variables
(defconst xapconst/gc-cons-percentage gc-cons-percentage "Default value of \"gc-cons-percentage\".")
(defconst xapconst/vc-handled-backends vc-handled-backends "Default value of \"vc-handled-backends\".")
(defconst xapconst/read-process-output (* 1024 1024 4) "Default value for \"read-process-output-max\" (4M).")
(defconst xapconst/file-name-handler-alist file-name-handler-alist "Default value of \"file-name-handler-alist\".")
(defconst xapconst/initial-background-color "#25272f" "Initial background color (before apply theme).")
(defconst xapconst/initial-foreground-color "#B3B9C4" "Initial foreground color (before apply theme).")

;; Garbage collector
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
  (lambda()
    (setq gc-cons-threshold xapconst/gc-cons-threshold
          gc-cons-percentage xapconst/gc-cons-percentage
          file-name-halder-alist xapconst/file-name-handler-alist
          vc-handled-backends xapconst/vc-handled-backends
          read-process-output-max xapconst/read-process-output)))

;; LSP Related config
(setenv "LSP_USE_PLISTS" "true")

;; Don't run GC on font caches
(setq inhibit-compacting-font-caches t)

  ;; Create foldder for auto-generated files if it does not exist
(unless (file-directory-p xapconst/autogen-folder-path)
  (make-directory xapconst/autogen-folder-path)
  (make-directory xapconst/elpa-path)
  (make-directory xapconst/eln-cache-path))

;; Setting package configuration.
(setq package-enable-at-startup nil) ; This is required by elpaca
(setq package-user-dir xapconst/elpa-path)


;; Redirect native compilation cache dir
;; (when (and (fboundp 'startup-redirect-eln-cache)
;;            (fboundp 'native-comp-available-p)
;;            (native-comp-available-p))
;;   (startup-redirect-eln-cache
;;    (convert-standard-filename xapconst/eln-cache-path)))

;; Set native compilation eln file path.
(when (native-comp-available-p)
  (setq native-comp-eln-load-path (cdr native-comp-eln-load-path))
  (defvar native-compile-target-directory xapconst/eln-cache-path)
  (add-to-list 'native-comp-eln-load-path xapconst/eln-cache-path)
  (startup-redirect-eln-cache xapconst/eln-cache-path)
  (defvar native-comp-async-report-warnings-errors nil))

;; Layout configuration ------------------------------------------------------------------------------------------------
(set-face-background 'default xapconst/initial-background-color)
(set-face-foreground 'default xapconst/initial-foreground-color)
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;;; early-init.el ends here.
