;;; use-bookmarks.el --- Bookmark functionality configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs bookmarks configuration

;;; Code:

(use-package emacs
  :ensure nil
  :general
  (xap/leader-key
    "b" '(nil :which-key "Bookmarks")
    "b a" '(bookmark-set :which-key "Add")
    "b l" '(list-bookmarks :which-key "List")
    "b j" '(bookmark-jump :which-key "Jump To")
    "b d" '(bookmark-delete :which-key "Delete")
    "b z" '(bookmark-delete-all :which-key "Delete All")))


(provide 'use-bookmarks)
;;; use-bookmarks.el ends here.
