;;; testonga-php.el --- Test executor helper for go on Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; This script might be renamed if I find the way to make it test agnostic.

;;; Code:

(require 'xap-utils)


(defgroup testonga-php nil
  "Testonga Go group."
  :prefix 'testonga-php
  :group 'editing)


(defcustom testonga-php/latest-test
  ""
  "Latest executed test command."
  :type 'string
  :group 'testonga-php)


;; Treesitter query
(defcustom testonga-php/ts-query
  "(method_declaration name: (name) @funcname (#match \"^test\" @funcname))"
  "Treesitter query to retrieve test on a golang test file."
  :type 'string
  :group 'testonga-php)


(defcustom testonga-php/test-command
  "php %svendor/bin/phpunit --config %sphpunit.xml --filter '%s\:\:%s' %s"
  "Command to be executed when select a test.
The first placeholder is the test path while the seocnd
is the name of the selected test."
  :type 'string
  :group 'testonga-php)


(defcustom testonga-php/capture-vars
  '("funcname")
  "List of capture var names from QUERY that needs to be capture."
  :type 'list
  :group 'testonga-php)


(defun testonga-php--test-name-from-capture (capture)
  "Return a test name string for the cli tool based on CAPTURE."
  (when (string-match-p "^\"" capture)
    (setq capture (replace-regexp-in-string "^\"\\|\"$" "" capture)
	  capture (string-replace " " "_" capture)
	  capture (concat ".*/" capture)))
  capture)


(defun testonga-php--test-path-from-buffer (file-path)
  "Replace FILE-PATH with a path valid to run test."
  (replace-regexp-in-string "/[^/]*" "/." file-path))

(defun testonga-php--get-buffer-name()
  "Get the clean file name without any addition from EMACS."
  (replace-regexp-in-string "<.*$" "" (buffer-file-name)))

(defun testonga-php--get-class-name()
  "Get the class name of the test based on the test file name."
  (replace-regexp-in-string ".php.*$" "" (buffer-name)))

(defun testonga-php-file()
  "Show all test in a Go test file in the minibuffer and execute upon selection."
  (interactive)
  (let* ((captures (xaputils/parse-query testonga-php/ts-query testonga-php/capture-vars))
	 (project-root (projectile-project-root))
	 (selected-test (completing-read "Select test to run: " captures)))
    (setq testonga-php/latest-test (format testonga-php/test-command
					   project-root
					   project-root
					   (testonga-php--get-class-name)
					   selected-test
					   (testonga-php--get-buffer-name)
					   ))
    (xaputils/exec-command
     testonga-php/latest-test
     "Testonga!")))


(defun testonga-php-latest()
  "Execute the latest executed test."
  (interactive)
  (xaputils/exec-command testonga-php/latest-test "Testonga!"))


(provide 'testonga-php)
;;; testonga-php.el ends here.
