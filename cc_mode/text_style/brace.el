;; File: brace.el
;; Created: 27, November 2013

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

(defun open-brace-with-auto-pairing ()
  (interactive)
  "Places an open brace then parses the current line from the beginning point of the
line to the position where the open brace was placed. The goal is to determine 
whether the situation is appropriate for a pairing closing brace with a semicolon to
be placed immediately after the opening brace. Situations where this behavior is
desired are when an `array' or an `enum' are currently being constructed. If the
current line does not satisfy the conditions previously stated. It will place an 
open brace normally and no further action will be initiated."
  (let ((enum_keyword) (open_square_bracket) (original_position) (face_type)
	(semicolon))
    (insert-char 123)
    (setq original_position (point))
	    
    (if (= (check-if-within-comment) false)
	(progn
	  (setq semicolon (search-forward-character ";" original_position
						    (beginning-of-line-point) -1))
	  (if (equal semicolon nil)
	      (progn
		(setq enum_keyword (search-forward-string "enum" (beginning-of-line-point)
							  original_position))
		(setq open_square_bracket (search-forward-character 
					   "[" (beginning-of-line-point)
					   original_position)))
	    (progn
	      (setq enum_keyword (search-forward-string "enum" 
							semicolon original_position))
	      (setq open_square_bracket (search-forward-character 
					 "[" semicolon original_position))))
	  
	  (if (or (and (not (equal enum_keyword nil)) (equal open_square_bracket nil))
		  (and (equal enum_keyword nil) (not (equal open_square_bracket nil))))
	      (progn
		(cond
		 ;; Enum keyword found
		 ((not (equal enum_keyword nil))
		  (setq face_type (text-property-any 
				   (beginning-of-line-point) original_position
				   'face 'font-lock-keyword-face)))
		 ;; Open square bracket found
		 ((not (equal open_square_bracket nil))
		  (setq face_type (text-property-any
				   (beginning-of-line-point) original_position
				   'face 'font-lock-type-face))))
		(if (not (equal face_type nil))
		    (progn
		      (insert-char 125)
		      (insert-char 59)
		      (backward-char)
		      (backward-char)))))))))

(defun close-brace-with-auto-handling ()
  (interactive)
  "Parses the current line if a closing brace should be placed. If pairing braces
were inserted as a result of the construction of an `array' or an `enum', a 
closing brace immediately after the cursor will be checked. If it exists the cursor
will automatically be moved after the closing brace; otherwise a closing brace will
be inserted. For all other cases a closing brace will be placed noramlly."
  (let ((enum_keyword) (open_square_bracket) (original_position) (face_type) 
        (semicolon))
    (setq original_position (point))

    (if (= (check-if-within-comment) true)
	(insert-char 125)
      (progn
	(setq semicolon (search-forward-character ";" original_position
						    (beginning-of-line-point) -1))
	(if (equal semicolon nil)
	    (progn
	      (setq enum_keyword (search-forward-string "enum" (beginning-of-line-point)
							original_position))
	      (setq open_square_bracket (search-forward-character 
					 "[" (beginning-of-line-point)
					 original_position)))
	  (progn
	    (setq enum_keyword (search-forward-string "enum" 
						      semicolon original_position))
	    (setq open_square_bracket (search-forward-character 
				       "[" semicolon original_position))))

	(if (or (and (not (equal enum_keyword nil)) (equal open_square_bracket nil))
		(and (equal enum_keyword nil) (not (equal open_square_bracket nil))))
	    (progn
	      (cond
	       ;; Enum keyword found
	       ((not (equal enum_keyword nil))
		(setq face_type (text-property-any
				 (beginning-of-line-point) original_position
				 'face 'font-lock-keyword-face)))
	       ;; Open square bracket found
	       ((not (equal open_square_bracket nil))
		(setq face_type (text-property-any
				 (beginning-of-line-point) original_position
				 'face 'font-lock-type-face))))
	      (if (not (equal face_type nil))
		  (if (= original_position (point-max))
		      (insert-char 125)
		    (progn
		      (if (string-equal "}" (char-to-string
					     (char-after original_position)))
			  (goto-char (+ original_position 1))
			(insert-char 125))))
		(insert-char 125)))
	  (insert-char 125))))))

(defun open-brace-with-auto-pairing-hook ()
  (define-key c-mode-base-map "{" 'open-brace-with-auto-pairing))

(defun close-brace-with-auto-handling-hook ()
  (define-key c-mode-base-map "}" 'close-brace-with-auto-handling))

(add-hook 'c-initialization-hook 'open-brace-with-auto-pairing-hook)
(add-hook 'c-initialization-hook 'close-brace-with-auto-handling-hook)
