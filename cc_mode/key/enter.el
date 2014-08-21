;; File: enter.el
;; Created: 09, December 2013

;; Copyright (C) Jarielle Catbagan
;;
;; BSD License
;;
;; Please refer to LICENSE.txt for license details

;; Dependencies

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

(defun block-closing ()
  (newline-and-indent)
  (newline)
  (insert-char 125)
  (indent-for-tab-command)
  (previous-line)
  (end-of-line))

(defun block-with-semicolon-closing ()
  (newline-and-indent)
  (newline)
  (insert-char 125)
  (insert-char 59)
  (indent-for-tab-command)
  (previous-line)
  (end-of-line))

(defun block-even ()
  (let ((position) (original_position) (open_brace) (close_brace) (search) (start) (stop)
	(open_brace_count) (close_brace_count) (return_value))
    (save-excursion
      (setq original_position (point))
      (setq start 1)
      (setq stop 0)
      (setq position original_position)
      (setq search start)
      (setq open_brace_count 0)
      (setq close_brace_count 0)
    
      (while (= search start)
	(setq open_brace (search-forward-character "{" position (point-min) -1))
	(if (not (equal open_brace nil))
	    (progn
	      (goto-char open_brace)
	      (if (= (check-if-within-comment) false)
		  (progn
		    (setq open_brace_count (+ open_brace_count 1))
		    (setq position open_brace))
		(setq position open_brace)))
	  (setq search stop)))

      (goto-char original_position)
      (setq search start)
      (setq position original_position)
      
      (while (= search start)
	(setq open_brace (search-forward-character "{" position (point-max) 1))
	(if (not (equal open_brace nil))
	    (progn
	      (goto-char open_brace)
	      (if (= (check-if-within-comment) false)
		  (progn
		    (setq open_brace_count (+ open_brace_count 1))
		    (setq position open_brace))
		(setq position open_brace)))
	  (setq search stop)))

      (goto-char original_position)
      (setq search start)
      (setq position original_position)

      (while (= search start)
	(setq close_brace (search-forward-character "}" position (point-min) -1))
	(if (not (equal close_brace nil))
	    (progn
	      (goto-char close_brace)
	      (if (= (check-if-within-comment) false)
		  (progn
		    (setq close_brace_count (+ close_brace_count 1))
		    (setq position close_brace))
		(setq position close_brace)))
	  (setq search stop)))

      (goto-char original_position)
      (setq search start)
      (setq position original_position)

      (while (= search start)
	(setq close_brace (search-forward-character "}" position (point-max) 1))
	(if (not (equal close_brace nil))
	    (progn
	      (goto-char close_brace)
	      (if (= (check-if-within-comment) false)
		  (progn
		    (setq close_brace_count (+ close_brace_count 1))
		    (setq position close_brace))
		(setq position close_brace)))
	  (setq search stop)))
      
      (cond
       ((= open_brace_count close_brace_count)
	(setq return_value true))
       ((> open_brace_count close_brace_count)
	(setq return_value false))
       ((< open_brace_count close_brace_count)
	(setq return_value false))))))

(defun enter-with-auto-completion ()
  (interactive)
    "For situations where either a statement block or a substatement block are in the 
process of being created, appropriately closing the blocks once the enter key has been 
pressed after the insertion of an open brace is desired. For all other cases, the enter 
key will operate normally with no further action initiated."
  (let ((position) (original_position) (linear_comment_delimiter) (continue)
	(semicolon) (open_brace) (data_type) (keyword_type) (struct_keyword) 
	(typedef_keyword) (enum_keyword) (class_keyword) (open_parentheses))
    (setq original_position (point))
    (setq position original_position)
    
    (if (= (check-if-within-comment) true)
        (progn
	  (setq linear_comment_delimiter (search-forward-string 
					  "//" position (beginning-of-line-point) -1))
	  (if (not (equal linear_comment_delimiter nil))
	      (setq continue true)
	    (setq continue false)))
      (setq continue true))

    (if (= continue false)
	(newline)
      (progn
	(if (not (equal linear_comment_delimiter nil))
	    (setq semicolon (search-forward-string ";" linear_comment_delimiter
						   (beginning-of-line-point) -1))
	  (setq semicolon (search-forward-string ";" position
						 (beginning-of-line-point) -1)))
	(cond
	 ((and (equal semicolon nil) (equal linear_comment_delimiter nil))
	  (setq open_brace (search-forward-string "{" position
						  (beginning-of-line-point) -1)))
	 ((and (equal semicolon nil) (not (equal linear_comment_delimiter nil)))
	  (setq open_brace (search-forward-string "{" linear_comment_delimiter
						  (beginning-of-line-point) -1)))
	 ((and (not (equal semicolon nil)) (equal linear_comment_delimiter nil))
	  (setq open_brace (search-forward-string "{" position semicolon -1)))
	 ((and (not (equal semicolon nil)) (not (equal linear_comment_delimiter nil)))
	  (setq open_brace (search-forward-string "{" linear_comment_delimiter
						  semicolon -1))))
	(cond
	 ((equal open_brace nil)
	  (setq data_type nil)
	  (setq keyword_type nil))
	 ((not (equal open_brace nil))
	  (cond 
	   ((and (equal semicolon nil) (equal linear_comment_delimiter nil))
	    (setq data_type (text-property-any position (beginning-of-line-point)
					       'face 'font-lock-type-face))
	    (setq keyword_type (text-property-any position (beginning-of-line-point)
						  'face 'font-lock-keyword-face)))
	   ((and (equal semicolon nil) (not (equal linear_comment_delimiter nil)))
	    (setq data_type (text-property-any linear_comment_delimiter
					       (beginning-of-line-point)
					       'face 'font-lock-type-face))
	    (setq keyword_type (text-property-any linear_comment_delimiter
						  (beginning-of-line-point)
						  'face 'font-lock-keyword-face)))
	   ((and (not (equal semicolon nil)) (equal linear_comment_delimiter nil))
	    (setq data_type (text-property-any position semicolon 
					       'face 'font-lock-type-face))
	    (setq keyword_type (text-property-any position semicolon
						  'face 'font-lock-keyword-face)))
	   ((and (not (equal semicolon nil)) (not (equal linear_comment_delimiter nil)))
	    (setq data_type (text-property-any linear_comment_delimiter semicolon
					       'face 'font-lock-type-face))
	    (setq keyword_type (text-property-any linear_comment_delimiter semicolon
						  'face 'font-lock-keyword-face))))
	  (if (and (equal data_type nil) (equal keyword_type nil))
	      (progn
		(cond
		 ((not (= (line-number-at-pos) 1))
		  (previous-line)
		  (end-of-line)
		  (setq position (point))
		  (if (= (check-if-within-comment) true)
		      (progn
			(setq linear_comment_delimiter (search-forward-string 
							"//" position
							(beginning-of-line-point) -1))
			(if (not (equal linear_comment_delimiter nil))
			    (setq continue true)
			  (setq continue false)))
		    (setq continue true))
		  
		  (if (= continue false)
		      (progn
			(setq data_type nil)
			(setq keyword_type nil))
		    (progn
		      (if (not (equal linear_comment_delimiter nil))
			  (setq semicolon (search-forward-string 
					   ";" linear_comment_delimiter
					   (beginning-of-line-point) -1))
			(setq semicolon (search-forward-string 
					 ";" position (beginning-of-line-point) -1)))
		      (cond 
		       ((and (equal semicolon nil) (equal linear_comment_delimiter nil))
			(setq data_type (text-property-any position 
							   (beginning-of-line-point)
							   'face 'font-lock-type-face))
			(setq keyword_type (text-property-any 
					    position (beginning-of-line-point)
					    'face 'font-lock-keyword-face)))
		       ((and (equal semicolon nil) (not (equal 
							 linear_comment_delimiter nil)))
			(setq data_type (text-property-any linear_comment_delimiter
							   (beginning-of-line-point)
							   'face 'font-lock-type-face))
			(setq keyword_type (text-property-any 
					    linear_comment_delimiter
					    (beginning-of-line-point)
					    'face 'font-lock-keyword-face)))
		       ((and (not (equal semicolon nil)) (equal 
							  linear_comment_delimiter nil))
			(setq data_type (text-property-any position semicolon 
							   'face 'font-lock-type-face))
			(setq keyword_type (text-property-any 
					    position semicolon
					    'face 'font-lock-keyword-face)))
		       ((and (not (equal semicolon nil)) 
			     (not (equal linear_comment_delimiter nil)))
			(setq data_type (text-property-any linear_comment_delimiter 
							   semicolon
							   'face 'font-lock-type-face))
			(setq keyword_type (text-property-any 
					    linear_comment_delimiter semicolon
					    'face 'font-lock-keyword-face)))))))
		 ((= (line-number-at-pos) 1)
		  (setq data_type nil)
		  (setq keyword_type nil)))))))
	(cond
	 ((and (equal data_type nil) (equal keyword_type nil))
	  (goto-char original_position)
	  (newline))
	 ((and (equal data_type nil) (not (equal keyword_type nil)))
	  (cond
	   ((and (equal semicolon nil) (equal linear_comment_delimiter nil))
	    (setq struct_keyword (search-forward-string "struct" position
							(beginning-of-line-point) -1))
	    (setq typedef_keyword (search-forward-string "typedef" position
						      (beginning-of-line-point) -1))
	    (setq enum_keyword (search-forward-string "enum" position
						      (beginning-of-line-point) -1))
	    (setq class_keyword (search-forward-string "class" position
						       (beginning-of-line-point) -1)))
	   ((and (equal semicolon nil) (not (equal linear_comment_delimiter nil)))
	    (setq struct_keyword (search-forward-string "struct" linear_comment_delimiter
							(beginning-of-line-point) -1))
	    (setq typedef_keyword (search-forward-string "typedef" linear_comment_delimiter
						      (beginning-of-line-point) -1))
	    (setq enum_keyword (search-forward-string "enum" linear_comment_delimiter
						      (beginning-of-line-point) -1))
	    (setq class_keyword (search-forward-string "class" linear_comment_delimiter
						       (beginning-of-line-point) -1)))
	   ((and (not (equal semicolon nil)) (equal linear_comment_delimiter nil))
	    (setq struct_keyword (search-forward-string "struct" position
							semicolon -1))
	    (setq typedef_keyword (search-forward-string "typedef" position
						      semicolon -1))
	    (setq enum_keyword (search-forward-string "enum" position
						      semicolon -1))
	    (setq class_keyword (search-forward-string "class" position
						       semicolon -1)))
	   ((and (not (equal semicolon nil)) (not (equal linear_comment_delimiter nil)))
	    (setq struct_keyword (search-forward-string "struct" linear_comment_delimiter
						        semicolon -1))
	    (setq typedef_keyword (search-forward-string "typedef" linear_comment_delimter
						      semicolon -1))
	    (setq enum_keyword (search-forward-string "enum" linear_comment_delimter
						      semicolon -1))
	    (setq class_keyword (search-forward-string "class" linear_comment_delimiter
						       semicolon -1))))
	  (cond
	   ((and (equal struct_keyword nil) (equal typedef_keyword nil)
		 (equal enum_keyword nil) (equal class_keyword nil))
	    (goto-char original_position)
	    (if (= (block-even) true)
		(progn
		  (newline)
		  (indent-for-tab-command))
	      (block-closing)))
	   ((or (and (not (equal struct_keyword nil)) (equal typedef_keyword nil)
		     (equal enum_keyword nil) (equal class_keyword nil))
		(and (not (equal struct_keyword nil)) (not (equal typedef_keyword nil))
		     (equal enum_keyword nil) (equal class_keyword nil))
		(and (equal struct_keyword nil) (equal typedef_keyword nil)
		     (equal enum_keyword nil) (not (equal class_keyword nil))))
	    (goto-char original_position)
	    (if (= (block-even) true)
		(progn
		  (newline)
		  (indent-for-tab-command))
	      (block-with-semicolon-closing)))
	   ((or (and (equal struct_keyword nil) (not (equal typedef_keyword nil))
		     (equal enum_keyword nil) (equal class_keyword nil))
		(and (equal struct_keyword nil) (equal typedef_keyword nil)
		     (not (equal enum_keyword nil)) (equal class_keyword nil))
		(and (equal struct_keyword nil) (not (equal typedef_keyword nil))
		     (not (equal enum_keyword nil)) (equal class_keyword nil)))
	    (goto-char original_position)
	    (newline))))
	 ((and (not (equal data_type nil)) (equal keyword_type nil))
	  (cond
	   ((and (equal semicolon nil) (equal linear_comment_delimiter nil))
	    (setq open_parentheses (search-forward-string "(" position
							  (beginning-of-line-point) -1)))
	   ((and (equal semicolon nil) (not (equal linear_comment_delimiter nil)))
	    (setq open_parentheses (search-forward-string "(" linear_comment_delimiter
						    (beginning-of-line-point) -1)))
	   ((and (not (equal semicolon nil)) (equal linear_comment_delimiter nil))
	    (setq open_parentheses (search-forward-string "(" position semicolon -1)))
	   ((and (not (equal semicolon nil)) (not (equal linear_comment_delimiter nil)))
	    (setq open_parentheses (search-forward-string "(" linear_comment_delimiter
							  semicolon -1))))
	  (goto-char original_position)
	  (if (equal open_parentheses nil)
	      (newline)
	    (progn
	      (if (= (block-even) true)
		  (progn
		    (newline)
		    (indent-for-tab-command))
		(block-closing)))))
	 ((and (not (equal data_type nil)) (not (equal keyword_type nil)))
	  (cond
	   ((and (equal semicolon nil) (equal linear_comment_delimiter nil))
	    (setq class_keyword (search-forward-string "class" position
						       (beginning-of-line-point) -1))
	    (setq struct_keyword (search-forward-string "struct" position
							(beginning-of-line-point) -1)))
	   ((and (equal semicolon nil) (not (equal linear_comment_delimiter nil)))
	    (setq class_keyword (search-forward-string "class" linear_comment_delimiter
						       (beginning-of-line-point) -1))
	    (setq struct_keyword (search-forward-string "struct" linear_comment_delimiter
							(beginning-of-line-point) -1)))
	   ((and (not (equal semicolon nil)) (equal linear_comment_delimiter nil))
	    (setq class_keyword (search-forward-string "class" position semicolon -1))
	    (setq struct_keyword (search-forward-string "struct" position
							semicolon -1)))
	   ((and (not (equal semicolon nil)) (not (equal linear_comment_delimiter nil)))
	    (setq class_keyword (search-forward-string "class" linear_comment_delimiter
						       semicolon -1))
	    (setq struct_keyword (search-forward-string "struct" linear_comment_delimiter
						        semicolon -1))))
	  (goto-char original_position)
	  (if (or (not (equal struct_keyword nil)) (not (equal class_keyword nil)))
	      (progn
		(if (= (block-even) true)
		    (progn
		      (newline)
		      (indent-for-tab-command))
		  (block-with-semicolon-closing)))
	    (block-closing))))))))

(defun enter-with-auto-completion-hook ()
  "Automatically determines whether the situation is appropriate to close a block
statement. For all other cases, a newline is inserted with no further action initiated."
  (define-key c-mode-base-map (kbd "RET") 'enter-with-auto-completion))

(add-hook 'c-initialization-hook 'enter-with-auto-completion-hook)
