;;; use-org-roam.el --- ORG ROAM configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Base org mode configuration

;;; Code:

(require 'custom-vars)

(use-package org-roam
  :ensure t
  :defer t
  :general
  (xap/leader-key
    "o r" '(nil :which-key "ORG Roam")
    "o r f" '(org-roam-node-find :which-key "Find/Create")
    "o r i" '(org-roam-node-insert :which-key "Insert")
    "o r c" '(org-roam-capture :which-key "Capture")
    "o r d" '(nil :which-key "Dailies")
    "o r d t" '(org-roam-dailies-goto-today :which-key "GoTo today")
    "o r d c" '(org-roam-dailies-capture-today :which-key "Capture today"))
  (xap/quick-leader
    "p" '(org-roam-dailies-capture-today :which-key "Roam Capture Today"))
  :init
  (unless (file-directory-p xapconst/org-roam-folder)
    (make-directory xapconst/org-roam-folder))
  :config
  (setq org-roam-capture-templates
	'(("d" "default" plain
	   "%?"
           :target (file+head "${slug}.org" "#+title: ${title}\n")
           :unnarrowed t))
	org-roam-directory xapconst/org-roam-folder
	org-roam-db-location xapconst/org-roam-db-path
	;; If you're using a vertical completion framework, you might want a more informative completion interface
	org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
	;; Roam Dailies configuration
	org-roam-dailies-directory xapconst/org-roam-dailies-dir)
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :ensure (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :defer t
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))

(provide 'use-org-roam)
;;; use-org-roam.el ends here.
