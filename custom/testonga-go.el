;;; testonga-go.el --- Test executor helper for go on Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Something something something

;;; Code:

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


(defun testonga-go--parse-query (query callback)
  "Parse the given QUERY and use CALLBACK on every capture group."
  (let* ((root-node (treesit-buffer-root-node))
	 (matches (treesit-query-capture root-node query)))
    (dolist (match matches)
      (let ((capture-var-name (car match))
	    (capture-text (treesit-node-text (cdr match))))
	(funcall callback capture-var-name capture-text)))))
	    

(defun testonga-go-capture-callback (match-name match-content)
  "After executing tree-sitter query use this call back on every MATCH-NAME and MATCH-CONTENT."
  (message
   "Inside the callback, the capture name is: %s and the capture value is %s"
   match-name
   match-content))


(defun testonga-go-file()
  "Show all test in a Go test file in the minibuffer and execute upon selection."
  (interactive)
  (testonga-go--parse-query testonga-go/ts-query #'testonga-go-capture-callback))



(provide 'testonga-go)
;;; testonga-go.el ends here.

