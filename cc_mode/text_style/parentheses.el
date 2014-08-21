;; File: parentheses.el
;; Created: 19, December 2013

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

(defun open-parentheses-with-auto-pairing ()
  (interactive)
  (if (= (check-if-within-comment) true)
      (insert-char 40)
    (progn
      (insert-char 40)
      (insert-char 41)
      (backward-char))))

(defun close-parentheses-with-auto-handling ()
  (interactive)
  (let ((original_position))
    (setq original_position (point))
    (if (= (check-if-within-comment) true)
	(insert-char 41)
      (progn
	(if (= original_position (point-max))
	    (insert-char 41)
	  (progn
	    (if (string-equal ")" (char-to-string (char-after original_position)))
		(goto-char (+ original_position 1))
	      (insert-char 41))))))))

(defun open-parentheses-with-auto-pairing-hook ()
  (define-key c-mode-base-map "(" 'open-parentheses-with-auto-pairing))

(defun close-parentheses-with-auto-handling-hook ()
  (define-key c-mode-base-map ")" 'close-parentheses-with-auto-handling))

(add-hook 'c-initialization-hook 'open-parentheses-with-auto-pairing-hook)
(add-hook 'c-initialization-hook 'close-parentheses-with-auto-handling-hook)
		  
	  
   
