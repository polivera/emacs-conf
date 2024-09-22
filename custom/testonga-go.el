;;; testonga-go.el --- Test executor helper for go on Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Something something something

;;; Code:

(defgroup testonga-go nil
  "Testonga Go group."
  :prefix 'testonga-go
  :group 'editing)

;; Declare some functions to make flycheck happy
(declare-function treesit-buffer-root-node "treesit")
(declare-function treesit-query-capture "treesit")
(declare-function treesit-node-text "treesit")

;; Treesitter query
(defvar testonga-go/ts-query
  "(function_declaration
       name: (identifier) @funcname (#match \"Test\" @funcname))
   (function_declaration
       name: (identifier) @funcname (#match \"Test\" @funcname)
       body: (block
           (expression_statement
	       (call_expression
	           function: (selector_expression field: (field_identifier) @flag (#match \"Run\" @flag))
		   arguments: (argument_list (interpreted_string_literal) @subtest)))))"
  "Treesitter query to retrieve test on a golang test file.")

(defcustom testonga-go--captures
  ()
  "List of captures for testing."
  :type 'list
  :group 'testonga-go)



;; One option that I have to achieve this is to put the add-to-list here
;; checking on every capture if the capture-var-name is IN-LIST
;; (kind of in_array) and then pass a curated list to the callback
;; I can also reverse the list here and concat the parent func name to the
;; subtests.
;; It seems that add-to-list reverse the result order.
(defun testonga-go--parse-query (query callback)
  "Parse the given QUERY and use CALLBACK on every capture group."
  (let* ((root-node (treesit-buffer-root-node))
	 (matches (treesit-query-capture root-node query)))
    (dolist (match matches)
      (let ((capture-var-name (car match))
	    (capture-text (treesit-node-text (cdr match))))
	(message "%s" capture-text)
	(funcall callback capture-var-name capture-text)))))
	    

(defun testonga-go/capture-callback (match-name match-content)
  "Testonga-go after capture callback.
After executing tree-sitter query use this callback on
every MATCH-NAME and MATCH-CONTENT."
  (when (or (string= match-name "funcname") (string= match-name "subtest"))
    (add-to-list 'testonga-go--captures match-content)))


(defun testonga-go-file()
  "Show all test in a Go test file in the minibuffer and execute upon selection."
  (interactive)
  (testonga-go--parse-query testonga-go/ts-query #'testonga-go/capture-callback)
  (dolist (item testonga-go--captures)
    (message "%s" item)))


(provide 'testonga-go)
;;; testonga-go.el ends here.

