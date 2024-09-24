;;; use-org.el --- ORG Mode configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Base org mode configuration

;;; Code:

(require 'custom-vars)

(use-package org
  :ensure t
  :defer t
  :general
  (xap/leader-key
    "o" '(nil :which-key "ORG")
    "o o" '(consult-outline :which-key "Outline"))

  :init
  (unless (file-directory-p xapconst/org-autogen-folder)
    (make-directory xapconst/org-autogen-folder))
  :config
  (setq org-src-preserve-indentation t
	org-startup-indented t
	org-return-follows-link t
	org-id-locations-file xapconst/org-id-locations-file)

;;   (setq org-id-locations-file (concat xap--org-junk-path "org-id-locations"))
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
    "o r f" '(org-roam-node-find :which-key "Outline")
    "o r i" '(org-roam-node-insert :which-key "Insert")
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






;; (use-package org
;;   :ensure t
;;   :defer t
;;   :general
;;   (xap/leader-key
;;     "o" '(nil :which-key "ORG")
;;     "o o" '(consult-outline :which-key "Outline"))
;;   :init
;;   (unless (file-directory-p xap--org-junk-path) (make-directory xap--org-junk-path))
;;   :config
;;   (setq org-id-locations-file (concat xap--org-junk-path "org-id-locations"))
;;   (setq org-src-preserve-indentation t)
;;   (setq org-startup-indented t)
;;   (dolist (face '((org-level-1 . 1.3)
;;                   (org-level-2 . 1.25)
;;                   (org-level-3 . 1.2)
;;                   (org-level-4 . 1.15)
;;                   (org-level-5 . 1.1)
;;                   (org-level-6 . 1.1)
;;                   (org-level-7 . 1.1)
;;                   (org-level-8 . 1.1)))
;;       (set-face-attribute (car face) nil :font xap--org-heading-font :weight 'regular :height (cdr face)))
;;   )

;; (when (file-directory-p xap--org-roam-folder)
;;   (use-package org-roam
;;     :ensure t
;;     :demand t
;;     :general
;;     (xap/leader-key
;;         "o r" '(nil :which-key "ORG Roam")
;;         "o r f" '(org-roam-node-find :which-key "Outline"))
;;     :config
;;     (setq org-roam-directory xap--org-roam-folder
;;           org-roam-complete-everywhere t
;;           org-roam-db-location xap--org-roam-db)
;;     (org-roam-db-autosync-mode)
;;   )
;; )

;; (provide 'use-org)




(provide 'use-org)
;;; use-org.el ends here.
