;; File: double_quote.el
;; Created: 20, December 2013

;; Copyright (C) Jarielle Catbagan
;;
;; BSD License
;;
;; Please refer to LICENSE.txt for license details

;; Dependencies:

;; - comment.el
;; - condition.el
;; - point.el
;; - search.el

;; Description:

;; Notes:

;;  1. This file is not part of the GNU Emacs distribution. To use the mechanisms 
;;     provided in this file, load this file along with any dependencies in the init
;;     file with correct paths.

;; Code:

(defun double-quote-with-auto-completion ()
  (interactive)
  "Determines how to insert a double quote depending on the current situation. In 
situations where a `string' or a `preprocessor' are created, insertion of a double quote
pair is desired. If a double quote pair has been inserted, the cursor is located in
between the pairs during the construction of a `string' or a `preprocessor', and the 
closing double quote is immediately after the cursor then the cursor will move after the
double quote. If it does not already exist or does not satisfy the situation previously
stated, then only one double quote will be inserted."
  (let ((original_position) (semicolon) (double_quote))
    (setq original_position (point))
    
    (if (= (check-if-within-comment) true)
	(insert-char 34)
      (progn
	(setq semicolon (search-forward-character ";" original_position
						  (beginning-of-line-point) -1))
	(if (not (equal semicolon nil))
	    (setq double_quote (search-forward-character "\"" original_position
							 semicolon -1))
	  (setq double_quote (search-forward-character "\"" original_position
						       (beginning-of-line-point) -1)))
	(if (equal double_quote nil)
	    (progn
	      (insert-char 34)
	      (insert-char 34)
	      (backward-char))
	  (progn
	    (if (= original_position (point-max))
		(insert-char 34)
	      (progn
		(if (string-equal "\"" (char-to-string (char-after original_position)))
		    (goto-char (+ original_position 1))
		  (insert-char 34))))))))))

(defun double-quote-with-auto-completion-hook ()
  (define-key c-mode-base-map "\"" 'double-quote-with-auto-completion))

(add-hook 'c-initialization-hook 'double-quote-with-auto-completion-hook)
    
