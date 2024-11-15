;;; use-dired.el --- Dired configuration configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Dired

;;; Code:

(use-package emacs
  :ensure nil
  :general
  (xap/quick-leader
    "d" '(dired-jump :which-key "Dired"))
  :config
  (when xapconst/is-macos
    ;; TODO: Fix this
    (let ((gls (executable-find "/opt/homebrew/bin/gls")))
      (when gls
	(setq dired-use-ls-dired t
              insert-directory-program gls))))
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-listing-switches "-aBhl  --group-directories-first")
  (defun xapfunc/dired-create-file-keymap ()
    "Custom keybindings for dired mode."
    (define-key dired-mode-map (kbd "M-+") 'dired-create-empty-file))

  (add-hook 'dired-mode-hook 'xapfunc/dired-create-file-keymap))

(provide 'use-dired)
;;; use-dired.el ends here.
