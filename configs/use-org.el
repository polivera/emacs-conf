;;; use-org.el --- ORG Mode configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Base org mode configuration

;;; Code:

(require 'custom-vars)

(defun xapfunc/org-agenda-open-personal()
  "Open personal agenda."
  (interactive)
  (find-file xapconst/personal-agenda-path))

(defun xapfunc/org-agenda-open-stayforlong()
  "Open stayforlong agenda."
  (interactive)
  (find-file xapconst/stayforlong-agenda-path))

(use-package org
  :ensure t
  :defer t
  :general
  (xap/leader-key
    "o" '(nil :which-key "ORG")
    "o o" '(consult-outline :which-key "Outline")
    "o a" '(nil :which-key "Agenda")
    "o a o" '(org-agenda :which-key "Open")
    "o a p" '(xapfunc/org-agenda-open-personal :which-key "Personal")
    "o a w" '(xapfunc/org-agenda-open-stayforlong :which-key "Stayforlong")
    )
  :init
  (unless (file-directory-p xapconst/org-autogen-folder)
    (make-directory xapconst/org-autogen-folder))
  (unless (file-directory-p xapconst/org-agenda-folder)
    (make-directory xapconst/org-agenda-folder))
  :config
  (setq org-src-preserve-indentation t
	org-startup-indented t
	org-return-follows-link t
	org-log-done 'time
	org-log-done-with-time t
	org-id-locations-file xapconst/org-id-locations-file
	org-agenda-files (list xapconst/personal-agenda-path
			       xapconst/stayforlong-agenda-path))
  (dolist (face '((org-level-1 . 1.3)
		  (org-level-2 . 1.25)
		  (org-level-3 . 1.2)
		  (org-level-4 . 1.15)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font xapconst/default-font-family :weight 'regular :height (cdr face))))

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


(provide 'use-org)
;;; use-org.el ends here.
