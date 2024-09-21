;;; testonga-go.el -*- lexical-binding: t; -*-
;;; Commentary: 
;;; Code:

(defvar testonga-go/ts-query
  "(function_declaration
       name: (identifier) @hl.fiery (#match \"Test\" @hl.fiery))
   (function_declaration
       name: (identifier) @hl.fiery (#match \"Test\" @hl.fiery)
       body: (block
           (expression_statement
	       (call_expression
	           function: (selector_expression field: (field_identifier) @foo (#match \"Run\" @foo))
		   arguments: (argument_list (interpreted_string_literal) @hl.gold)))))"
  "Treesitter query to retrieve test on a golang test file")
