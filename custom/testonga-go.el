;;; testonga-go.el --- Test executor helper for go on Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; This script might be renamed if I find the way to make it test agnostic.

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

(defcustom testonga-go/capture-vars
  '("funcname" "subtest")
  "List of capture var names from QUERY that needs to be capture."
  :type 'list
  :group 'testonga-go)

(defun testonga-go--parse-query (query capture-list)
  "Parse QUERY and return a list of captured elements that match CAPTURE-LIST."
  (let* ((root-node (treesit-buffer-root-node))
	 (matches (treesit-query-capture root-node query))
	 (captures ()))
    (dolist (match matches)
      (let ((capture-var-name (car match))
	    (capture-text (treesit-node-text (cdr match) 'no-properties)))
	(when (member-ignore-case (format "%s" capture-var-name) capture-list)
	  (cl-pushnew (format "%s" capture-text) captures :test #'string=))))
    captures))


(defun testonga-go--run-capture (capture)
  "Execute test that math CAPTURE and echo the result into a new buffer."
  (message "this will execute %s" (string-replace "\"" "" capture)))


;;; Investigate the following method
(defun testonga-go--exec-command (command)
  "Execute a shell COMMAND and display the output in a temporary buffer.
The buffer should be able to close using the \"q\" key."
  (let ((buffer (get-buffer-create "Testonga!"))) ;; Create or get the temp buffer
    (with-current-buffer buffer
      (read-only-mode 0)
      (erase-buffer)
      (insert (format "Running command: %s\n" command))
      (read-only-mode 1)
      (special-mode))
    (start-process-shell-command "mycommand" buffer command)
    (pop-to-buffer buffer)))

(defun testonga-go--test-from-capture (capture)
  "Return a test name string for the cli tool based on CAPTURE."
  (when (string-match-p "^\"" capture)
    (setq capture (replace-regexp-in-string "^\"\\|\"$" "" capture)
	  capture (string-replace " " "_" capture)
	  capture (concat ".*/" capture)))
  capture)

(defun testonga-go-file()
  "Show all test in a Go test file in the minibuffer and execute upon selection."
  (interactive)
  (let* ((captures (testonga-go--parse-query testonga-go/ts-query testonga-go/capture-vars))
	 (selected-test (completing-read "Select test to run: " captures)))
    (testonga-go--exec-command
     (format
      "go test -tags=unit,integration,e2e -v %s -run '%s'"
      buffer-file-name
      (testonga-go--test-from-capture selected-test)))))






(provide 'testonga-go)
;;; testonga-go.el ends here.

