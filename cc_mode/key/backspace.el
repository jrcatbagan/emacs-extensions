;; File: backspace.el
;; Created: 22, December 2013

;; Copyright (C) Jarielle Catbagan
;;
;; BSD License
;;
;; Please refer to LICENSE.txt for license details

;; Dependencies

;; - comment.el
;; - condition.el

;; Description:

;; Notes:

;;  1. This file is not part of the GNU Emacs distribution. To use the mechanisms 
;;     provided in this file, load this file along with any dependencies in the init
;;     file with correct paths.

;; Codes:

(defun backspace-with-auto-deletion ()
  (interactive)
  "Deletes the previous character. If any of the characters that have a matching pair is 
immediately before the cursor and the other matching pair is immediately after the cursor,
then the pair is deleted. For the brace, there are situations where a semicolon is 
appeneded after the close brace; this is also deleted as well."
  (let ((original_position))
    (setq original_position (point))
    (if (= (check-if-within-comment) true)
	(delete-backward-char 1)
      (progn
	(if (or (= original_position (point-min)) (= original_position (point-max)))
	    (delete-backward-char 1)
	  (progn
	    (if (or (string-equal "{" (char-to-string (char-before original_position)))
		    (string-equal "[" (char-to-string (char-before original_position)))
		    (string-equal "(" (char-to-string (char-before original_position)))
		    (string-equal "<" (char-to-string (char-before original_position)))
		    (string-equal "\"" (char-to-string (char-before original_position)))
		    (string-equal "'" (char-to-string (char-before original_position))))
		(cond
		 ((string-equal "{" (char-to-string (char-before original_position)))
		  (if (string-equal "}" (char-to-string (char-after original_position)))
		      (progn
			(if (not (= (+ original_position 1) (point-max)))
			    (if (string-equal ";" (char-to-string 
						   (char-after (+ original_position 1))))
				(progn
				  (delete-forward-char 2)
				  (delete-backward-char 1))
			      (progn
				(delete-forward-char 1)
				(delete-backward-char 1)))
			  (progn
			    (delete-forward-char 1)
			    (delete-backward-char 1))))
		    (delete-backward-char 1)))
		 ((string-equal "[" (char-to-string (char-before original_position)))
		  (if (string-equal "]" (char-to-string (char-after original_position)))
		      (progn
			(delete-forward-char 1)
			(delete-backward-char 1))
		    (delete-backward-char 1)))
		 ((string-equal "(" (char-to-string (char-before original_position)))
		  (if (string-equal ")" (char-to-string (char-after original_position)))
		      (progn
			(delete-forward-char 1)
			(delete-backward-char 1))
		    (delete-backward-char 1)))
		 ((string-equal "<" (char-to-string (char-before original_position)))
		  (if (string-equal ">" (char-to-string (char-after original_position)))
		      (progn
			(delete-forward-char 1)
			(delete-backward-char 1))
		    (delete-backward-char 1)))
		 ((string-equal "\"" (char-to-string (char-before original_position)))
		  (if (string-equal "\"" (char-to-string (char-after original_position)))
		      (progn
			(delete-forward-char 1)
			(delete-backward-char 1))
		    (delete-backward-char 1)))
		 ((string-equal "'" (char-to-string (char-before original_position)))
		  (if (string-equal "'" (char-to-string (char-after original_position)))
		      (progn
			(delete-forward-char 1)
			(delete-backward-char 1))
		    (delete-backward-char 1))))
	      (progn
		(delete-backward-char 1)))))))))
	  
(defun backspace-with-auto-deletion-hook ()
  (define-key c-mode-base-map (kbd "DEL") 'backspace-with-auto-deletion))

(add-hook 'c-initialization-hook 'backspace-with-auto-deletion-hook)
