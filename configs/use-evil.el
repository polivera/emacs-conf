;;; use-evil.el --- Evil mode configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Evil mode configuration.

;;; Code:

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-echo-state nil)
  (setq save-interprogram-paste-before-kill t)
  (setq evil-want-C-u-scroll t)
  :general
  (xap/quick-leader
    "\\" '(evil-window-vsplit :which-key "Vertical Split"))
  :config
  ;; Evil define custom functions for deleting and changing without adding content to the kill ring
  (evil-define-operator xapfunc/evil-delete-no-kill-ring (beg end type register yank-handler)
    "Delete text from BEG to END with TYPE. Do not save it in any register."
    (interactive "<R><x><y>")
    (evil-delete beg end type ?_ yank-handler))

  (evil-define-operator xapfunc/evil-delete-line-no-kill-ring (beg end type register yank-handler)
    "Delete to end of line. Do not save it in any register."
    :motion evil-end-of-line-or-visual-line
    (interactive "<R><x>")
    (evil-delete-line beg end type ?_ yank-handler))

  (evil-define-operator xapfunc/evil-change-no-kill-ring (beg end type register yank-handler delete-func)
    "Change text from BEG to END with TYPE. Do not save it in any registry."
    (interactive "<R><x><y>")
    (evil-change beg end type ?_ yank-handler))

  (evil-define-operator xapfunc/evil-change-line-no-kill-ring (beg end type register yank-handler)
    "Change to end of line, or change whole line if characterwise visual mode."
    :motion evil-end-of-line-or-visual-line
    (interactive "<R><x><y>")
    (evil-change-line beg end type ?_ yank-handler))

  ;; Activate evil mode
  (evil-mode 1)

  ;; Unbind RET key
  (define-key evil-motion-state-map (kbd "RET") nil)

  ;; Normal mode keymaps
  (evil-global-set-key 'normal (kbd "d") 'xapfunc/evil-delete-no-kill-ring)
  (evil-global-set-key 'normal (kbd "D") 'xapfunc/evil-delete-line-no-kill-ring)
  (evil-global-set-key 'normal (kbd "m") 'evil-delete)
  (evil-global-set-key 'normal (kbd "M") 'evil-delete-line)
  (evil-global-set-key 'normal (kbd "c") 'xapfunc/evil-change-no-kill-ring)
  (evil-global-set-key 'normal (kbd "C") 'xapfunc/evil-change-line-no-kill-ring)

  ;; Visual mode keymaps
  (evil-global-set-key 'visual (kbd "d") 'xapfunc/evil-delete-no-kill-ring)
  (evil-global-set-key 'visual (kbd "D") 'xapfunc/evil-delete-line-no-kill-ring)
  (evil-global-set-key 'visual (kbd "m") 'evil-delete)
  (evil-global-set-key 'visual (kbd "M") 'evil-delete-line)
  (evil-global-set-key 'visual (kbd "c") 'xapfunc/evil-change-no-kill-ring)
  (evil-global-set-key 'visual (kbd "C") 'xapfunc/evil-change-line-no-kill-ring)

  ;; Insert mode keymaps
  (evil-global-set-key 'insert (kbd "C-g") 'evil-normal-state)
  (evil-global-set-key 'normal (kbd "tu") 'evil-upcase)
  (evil-global-set-key 'normal (kbd "tl") 'evil-downcase)
)

(use-package evil-collection
  :ensure t
  :demand t
  :config (evil-collection-init))


(provide 'use-evil)
;;; use-evil.el ends here.
