;; File comment.el
;; Created:: 16, December 2013

;; Copyright (C) Jarielle Catbagan
;;
;; BSD License
;;
;; Please refer to LICENSE.txt for license details

;; Dependencies:

;; - condition.el
;; - point.el
;; - search.el

;; Description:

;; The functions located in this file provide a mechanism to determine whether the 
;; current point lies within a comment block or is part of a linear comment.

;; Code:

(defun check-if-within-comment ()
  "Determines if the current cursor position at the point of analysis is located within
a comment. If so false is returned; otherwise true is returned."
  (let ((original_position) (face_type) (return_value) (semicolon) 
	(linear_comment_delimiter) (open_block_comment_delimiter)
	(close_block_comment_delimiter))
    
    (setq original_position (point))
    
    (if (= original_position (point-max))
	(progn
	  (setq semicolon (search-forward-character ";" original_position
						    (beginning-of-line-point) -1))
	  (if (not (equal semicolon nil))
	      (if (equal 'font-lock-comment-face (get-text-property semicolon 'face))
		  (setq semicolon nil)))
	  (if (not (equal semicolon nil))
	      (progn
		(setq linear_comment_delimiter (search-forward-string 
						"//" original_position semicolon -1))
		(setq open_block_comment_delimiter (search-forward-string 
						    "/*" original_position semicolon -1))
		(setq close_block_comment_delimiter (search-forward-string 
						     "*/" original_position semicolon -1)))
	    (progn
	      (setq linear_comment_delimiter (search-forward-string 
					      "//"  original_position
					      (beginning-of-line-point) -1))
	      (setq open_block_comment_delimiter (search-forward-string
						  "/*" original_position
						  (beginning-of-line-point) -1))
	      (setq close_block_comment_delimiter (search-forward-string
						   "*/" original_position
						   (beginning-of-line-point) -1))))
	  (cond
	   ((and (equal linear_comment_delimiter nil)
		 (equal open_block_comment_delimiter nil)
		 (equal close_block_comment_delimiter nil))
	    (setq return_value false))
	   ((and (not (equal linear_comment_delimiter nil))
		 (equal open_block_comment_delimiter nil)
		 (equal close_block_comment_delimiter nil))
	    (setq return_value true))
	   ((and (equal linear_comment_delimiter nil)
		 (not (equal open_block_comment_delimiter nil))
		 (equal close_block_comment_delimiter nil))
	    (setq return_value true))
	   ((and (equal linear_comment_delimiter nil)
		 (equal open_block_comment_delimiter nil)
		 (not (equal close_block_comment_delimiter nil)))
	    (setq return_value false))
	   ((and (not (equal linear_comment_delimiter nil))
		 (not (equal open_block_comment_delimiter nil))
		 (equal close_block_comment_delimiter nil))
	    (setq return_value true))
	   ((and (equal linear_comment_delimiter nil)
		 (not (equal open_block_comment_delimiter nil))
		 (not (equal close_block_comment_delimiter nil)))
	    (if (> open_block_comment_delimiter close_block_comment_delimiter)
		(setq return_value true)
	      (setq return_value false)))
	   ((and (not (equal linear_comment_delimiter nil))
		 (not (equal open_block_comment_delimiter nil))
		 (not (equal close_block_comment_delimiter nil)))
	    (setq return_value true))
	   ((and (not (equal linear_comment_delimiter nil))
		 (equal open_block_comment_delimiter nil)
		 (not (equal close_block_comment_delimiter nil)))
	    (if (> linear_comment_delimiter close_block_comment_delimiter)
		(setq return_value true)
	      (setq return_value false)))))

      (progn
        (setq face_type (get-text-property original_position 'face))
	(if (equal 'font-lock-comment-face face_type)
	    (setq return_value true)
	  (setq return_value false))))))

