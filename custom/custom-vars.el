;;; custom-vars.el --- Custom variables definition

;;; Commentary:

;; Set custom variables to use thourout Emacs configuration

;;; Code:

;; Base autogen path
(defconst xapconst/autogen-folder-path (concat user-emacs-directory "autogen/") "Path to auto generated files.")

;;; CheckOS
(defconst xapconst/is-linux (eq system-type 'gnu/linux) "Set to t if it is a linux system, nil otherwise.")
(defconst xapconst/is-macos (eq system-type 'darwin) "Set to t if it is a mac os system, nil otherwise.")
(defconst xapconst/is-window (eq system-type 'windows-nt) "Set to t if it is a windows system, nil otherwise.")

;;; Custom fonts
(defconst xapconst/default-font-family "IosevkaTerm Nerd Font" "Default font family.")
(defvar xapvar/default-font-size 130 "Default Emacs font size.")
(when xapconst/is-linux
  (setq xapvar/default-font-size 125))
(when xapconst/is-macos
  (setq xapvar/default-font-size 145))

(defconst xapconst/elpa-path (concat xapconst/autogen-folder-path "elpa/") "Path to elpa directory inside autogen folder.")
(defconst xapconst/eln-cache-path (concat xapconst/autogen-folder-path "eln-cache/") "Path to eln cache direcory.")
(defconst xapconst/transient-history (concat xapconst/autogen-folder-path "transient-history.el") "Set path to transient-history-file.")
(defconst xapconst/recentf-autogen-file (concat xapconst/autogen-folder-path "recentf") "Recentf db file path.")
(defconst xapconst/savehist-autogen-file (concat xapconst/autogen-folder-path "savehist") "Savehist files path.")
(defconst xapconst/places-autogen-file (concat xapconst/autogen-folder-path "places") "Places files path.")
(defconst xapconst/auto-saves-path (concat xapconst/autogen-folder-path "auto-saves/") "Path to folder that contains autosave data.")
(defconst xapconst/file-name-handler-alist file-name-handler-alist "Default value of \"file-name-handler-alist\".")

;;; Projectile
(defconst xapconst/projectile-bookmarks-file (concat xapconst/autogen-folder-path "projectile-bookmarks.eld") "Path to projectile known projects file.")
(defconst xapconst/projectile-cache-file (concat xapconst/autogen-folder-path "projectile.cache") "Path to projectile cache file.")

;; LSP
(defconst xapconst/lsp-session-file (concat xapconst/autogen-folder-path "lsp-session-v1") "Path to lsp session file.")

;; ORG
(defconst xapconst/org-autogen-folder (concat xapconst/autogen-folder-path "org/"))
(defconst xapconst/org-id-locations-file (concat xapconst/org-autogen-folder "org-id-locations"))
(defconst xapconst/org-roam-folder (file-truename "~/Documents/OrgNotes/"))
(defconst xapconst/org-roam-dailies-dir (concat xapconst/org-roam-folder "dailies/"))
(defconst xapconst/org-roam-db-path (concat xapconst/autogen-folder-path "org-roam.db"))


(provide 'custom-vars)
;;; custom-vars.el ends here.
