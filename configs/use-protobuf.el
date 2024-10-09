;;; use-makefile.el --- Makefile specific configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration for makefiles

;;; Code:

(use-package protobuf-ts-mode
  :ensure (:host github :repo "emacsattic/protobuf-ts-mode")
  :demand t)

(use-package emacs
  :ensure nil
  :config
    ;; Automatically associate with .proto files
    (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-ts-mode)))

(provide 'use-protobuf)
;;; use-protobuf.el ends here.
