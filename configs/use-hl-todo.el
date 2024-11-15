;;; use-hl-todo.el --- Magit configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; HL-Todo Highlight TODOs
;; @see: https://github.com/tarsius/hl-todo

;;; Code:

;; @todo: somehting off
(use-package hl-todo
  :ensure t
  :demand t
  :config
  (setq hl-todo-keyword-faces
	'(("TODO"   . "#FF6347")
          ("todo:"  . "#FF4500")
          ("FIXME"  . "#FF4500")
          ("NOTE"   . "#1E90FF")
          ("HACK"   . "#FFD700")
          ("REVIEW" . "#00FF00")
          ("OPTIMIZE" . "#00CED1")))

  (global-hl-todo-mode)
)


(message (regexp-quote "todo:"))


(provide 'use-hl-todo)
;;; use-hl-todo.el ends here.
