;;; use-treesit.el --- Treesitter configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal configuration for treesitter related packages

;;; Code:

(use-package treesit
  :ensure nil
  :demand t
  :config
  (setq treesit-font-lock-level 4)
  ;; Add grammar list
  (dolist (grammar
	   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.20.5"))
             (css "https://github.com/tree-sitter/tree-sitter-css")
             (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	     (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0" "src"))
             (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
             (html "https://github.com/tree-sitter/tree-sitter-html")
             (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	     (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
	     (php . ("https://github.com/tree-sitter/tree-sitter-php" "v0.20.0" "php/src"))
             (python "https://github.com/tree-sitter/tree-sitter-python")
             (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
             (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

    ;; Add to the source list
    (add-to-list 'treesit-language-source-alist grammar)

    ;; Install languages
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar))))

  ;; Remap major modes
  (dolist (mapping
	   '((css-mode . css-ts-mode)
	     (go-mode . go-ts-mode)
	     (bash-mode . bash-ts-mode)
	     (json-mode . json-ts-mode)
	     (php-mode . php-ts-mode)
	     (javascript-mode . javascript-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))
  

;;; Treesit helper
(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :demand t
  :hook
  ((prog-mode . combobulate-mode)))


(provide 'use-treesit)
;;; use-treesit.el ends here.
