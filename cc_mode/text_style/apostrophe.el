;; File: apostrophe.el
;; Created:: 20, December 2013

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

;; Description::

;; Notes::

;;  1. This file is not part of the GNU Emacs distribution. To use the mechanisms 
;;     provided in this file, load this file along with any dependencies in the init
;;     file with correct paths.

;; Code:

(defun apostrophe-with-auto-pairing ()
  (interactive)
  "Inserts a pair of apostrophes. If an apostrophe already exists before the cursor 
and an apostrophe that pairs with the first apostrophe exists immediately after the 
cursor, then the cursor will move one point forward. If the current cursor position
is located within a comment, only one apostrophe will be inserted."
  (let ((original_position) (semicolon) (apostrophe))
    (setq original_position (point))
    
    (if (= (check-if-within-comment) true)
	(insert-char 39)
      (progn
	(setq semicolon (search-forward-character ";" original_position
						  (beginning-of-line-point) -1))
	(if (not (equal semicolon nil))
	    (setq apostrophe (search-forward-character "'" original_position
						       semicolon -1))
	  (setq apostrophe (search-forward-character "'" original_position
						     (beginning-of-line-point) -1)))
	(if (equal apostrophe nil)
	    (progn
	      (insert-char 39)
	      (insert-char 39)
	      (backward-char))
	  (progn
	    (if (= original_position (point-max))
		(insert-char 39)
	      (progn
		(if (string-equal "'" (char-to-string (char-after original_position)))
		    (goto-char (+ original_position 1))
		  (insert-char 39))))))))))

(defun apostrophe-with-auto-pairing-hook ()
  (define-key c-mode-base-map "'" 'apostrophe-with-auto-pairing))

(add-hook 'c-initialization-hook 'apostrophe-with-auto-pairing-hook)
