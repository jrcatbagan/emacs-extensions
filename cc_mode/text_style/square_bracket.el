;; File: square_bracket.el
;; Created: 01, December 2013

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

(defun open-square-bracket-with-auto-pairing ()
  (interactive)
  "Places an open square bracket. After insertion, the current line is analyzed if
the situation is appropriate to place a pairing close square bracket. The only situation
where this behavior is desired is when an `array' is currently being constructed."
  (let ((original_position) (face_type) (semicolon))
    (insert-char 91)
    (setq original_position (point))
    
    (if (= (check-if-within-comment) false)
	(progn
	  (setq semicolon (search-forward-character ";" original_position
						    (beginning-of-line-point) -1))
	  (if (equal semicolon nil)
	      (setq face_type (text-property-any
			       (beginning-of-line-point) original_position
			       'face 'font-lock-type-face))
	    (setq face_type (text-property-any
			     semicolon original_position
			     'face 'font-lock-type-face)))
	  (if (not (equal face_type nil))
	      (progn
		(insert-char 93)
		(backward-char)))))))

(defun close-square-bracket-with-auto-handling ()
  (interactive)
  "Determines whether to place a close square bracket or not. If an `array' is currently
being constructed and a closing square bracket exists immediately after the current 
cursor position, then the cursor will move after the close square bracket. In all other
cases, a close square bracket will be inserted with no further action required."
  (let ((open_square_bracket) (original_position) (face_type))
    (setq original_position (point))

    (if (= (check-if-within-comment) true)
	(insert-char 93)
      (progn
	(setq semicolon (search-forward-character ";" original_position
						  (beginning-of-line-point) -1)) 
	(if (equal semicolon nil)
	    (progn
	      (setq open_square_bracket (search-forward-string "[" 
							       (beginning-of-line-point)
							       original_position))
	      (setq face_type (text-property-any (beginning-of-line-point)
						 original_position
						 'face 'font-lock-type-face)))
	  (progn
	    (setq open_square_bracket (search-forward-string "[" semicolon
							     original_position))
	    (setq face_type (text-property-any semicolon original_position
					       'face 'font-lock-type-face))))
	
	(if (and (not (equal face_type nil)) (not (equal open_square_bracket nil)))
	    (progn
	      (if (= original_position (point-max))
		  (insert-char 93)
		(progn
		  (if (string-equal "]" (char-to-string (char-after original_position)))
		      (goto-char (+ original_position 1))
		    (insert-char 93)))))
	  (insert-char 93))))))

  
(defun open-square-bracket-with-auto-pairing-hook ()
  (define-key c-mode-base-map "[" 'open-square-bracket-with-auto-pairing))

(defun close-square-bracket-with-auto-handling-hook ()
  (define-key c-mode-base-map "]" 'close-square-bracket-with-auto-handling))

(add-hook 'c-initialization-hook 'open-square-bracket-with-auto-pairing-hook)
(add-hook 'c-initialization-hook 'close-square-bracket-with-auto-handling-hook)
