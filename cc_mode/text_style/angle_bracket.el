;; File: angle_bracket.el
;; Created: 08, December 2013

;; Copyright (C) Jarielle Catbagan
;;
;; BSD License
;;
;; Please refer to LICENSE.txt for license details

;; Dependencies:

;; - condition.el
;; - point.el

;; Description:

;; Notes:

;;  1. This file is not part of the GNU Emacs distribution. To use the mechanisms 
;;     provided in this file, load this file along with any dependencies in the init
;;     file with correct paths.

;; Code

(defun open-angle-bracket-with-auto-pairing ()
  (interactive)
  "Places an open angle bracket and determines whether the situation is appropriate 
to place a pairing close angle bracket. Such situations where this behavior is desired
is when a `preprocessor' is currently being constructed. In all other cases no
further action is initated beyond placing the open angle bracket."
  (let ((preprocessor) (hash) (include_keyword) (original_position))
    (setq original_position (point))

    (if (not (= (check-if-within-comment) true))
	(progn
	  (setq hash (search-forward-string "#" (beginning-of-line-point)
					    original_position))
	  (setq include_keyword (search-forward-string "include" (beginning-of-line-point)
						       original_position))
	  (cond
	   ((and (equal hash nil) (equal include_keyword nil))
	    (insert-char 60))
	   ((and (equal hash nil) (not (equal include_keyword nil)))
	    (insert-char 60))
	   ((and (not (equal hash nil)) (equal include_keyword nil))
	    (insert-char 60))
	   ((and (not (equal hash nil)) (not (equal include_keyword nil)))
	    (if (< hash include_keyword)
		(progn
		  (insert-char 60)
		  (insert-char 62)
		  (backward-char))
	      (insert-char 60))))))))

(defun close-angle-bracket-with-auto-handling ()
  (interactive)
  "Determines how to place a close angle bracket. Situations where consideration should
be taken into account are when a close angle bracket is placed automatically when an
open angle bracket is inserted during a `preprocessor' declaration. In this case, if a
close angle bracket exists immediately after the cursor, the cursor will move after the
angle bracket. In all other cases, a close angle bracket will be inserted with no 
further action initiated."
  (let ((preprocessor) (original_position))
    (setq preprocessor (text-property-any (beginning-of-line-point) (point)
					  'face 'font-lock-preprocessor-face))
    (if (equal preprocessor nil)
	(insert-char 62)
      (progn
	(setq original_position (point))
	(if (= original_position (point-max))
	    (insert-char 62)
	  (progn
	    (if (string-equal ">" (char-to-string (char-after original_position)))
		(goto-char (+ original_position 1))
	      (insert-char 62))))))))

(defun open-angle-bracket-with-auto-pairing-hook ()
  (define-key c-mode-base-map "<" 'open-angle-bracket-with-auto-pairing))

(defun close-angle-bracket-with-auto-handling-hook ()
  (define-key c-mode-base-map ">" 'close-angle-bracket-with-auto-handling))

(add-hook 'c-initialization-hook 'open-angle-bracket-with-auto-pairing-hook)
(add-hook 'c-initialization-hook 'close-angle-bracket-with-auto-handling-hook)
