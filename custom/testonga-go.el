;;; testonga-go.el --- Test executor helper for go on Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; This script might be renamed if I find the way to make it test agnostic.

;;; Code:

(require 'xap-utils)


(defgroup testonga-go nil
  "Testonga Go group."
  :prefix 'testonga-go
  :group 'editing)


(defcustom testonga-go/latest-test
  ""
  "Latest executed test command."
  :type 'string
  :group 'testonga-go)


;; Treesitter query
(defcustom testonga-go/ts-query
  "(function_declaration
       name: (identifier) @funcname (#match \"Test\" @funcname))
   (function_declaration
       name: (identifier) @funcname (#match \"Test\" @funcname)
       body: (block
           (expression_statement
	       (call_expression
	           function: (selector_expression field: (field_identifier) @flag (#match \"Run\" @flag))
		   arguments: (argument_list (interpreted_string_literal) @subtest)))))"
  "Treesitter query to retrieve test on a golang test file."
  :type 'string
  :group 'testonga-go)


(defcustom testonga-go/test-command
  "go test -tags=unit,integration,e2e -v %s -run '%s'"
  "Command to be executed when select a test.
The first placeholder is the test path while the seocnd
is the name of the selected test."
  :type 'string
  :group 'testonga-go)


(defcustom testonga-go/capture-vars
  '("funcname" "subtest")
  "List of capture var names from QUERY that needs to be capture."
  :type 'list
  :group 'testonga-go)


(defun testonga-go--test-name-from-capture (capture)
  "Return a test name string for the cli tool based on CAPTURE."
  (when (string-match-p "^\"" capture)
    (setq capture (replace-regexp-in-string "^\"\\|\"$" "" capture)
	  capture (string-replace " " "_" capture)
	  capture (concat ".*/" capture)))
  capture)


(defun testonga-go--test-path-from-buffer (file-path)
  "Replace FILE-PATH with a path valid to run test."
  (replace-regexp-in-string "/[^/]*_test.go" "/." file-path))


(defun testonga-go-file()
  "Show all test in a Go test file in the minibuffer and execute upon selection."
  (interactive)
  (let* ((captures (xaputils/parse-query testonga-go/ts-query testonga-go/capture-vars))
	 (selected-test (completing-read "Select test to run: " captures)))
    (setq testonga-go/latest-test (format testonga-go/test-command
				      (testonga-go--test-path-from-buffer buffer-file-name)
				      (testonga-go--test-name-from-capture selected-test)))
    (xaputils/exec-command
     testonga-go/latest-test
     "Testonga!")))


(defun testonga-go-latest()
  "Execute the latest executed test."
  (interactive)
  (xaputils/exec-command testonga-go/latest-test "Testonga!"))


(provide 'testonga-go)
;;; testonga-go.el ends here.
