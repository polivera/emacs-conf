;;; use-dired.el --- Dired configuration configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Dired

;;; Code:

(use-package emacs
  :ensure nil
  :config
  (when xapconst/is-macos
    (let ((gls (executable-find "gls")))
      (when gls
	(setq dired-use-ls-dired t
              insert-directory-program gls))))

  (setq dired-listing-switches "-aBhl  --group-directories-first"))

(provide 'use-dired)
;;; use-dired.el ends here.
