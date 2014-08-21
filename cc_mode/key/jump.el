;; File: jump.el
;; Created: 22, December 2013

;; Copyright (C) Jarielle Catbagan
;;
;; BSD License
;;
;; Please refer to LICENSE.txt for license details

;; Dependencies:

;; - comment,el
;; - condition.el
;; - search.el

;; Description:

;; Notes:

;;  1. This file is not part of the GNU Emacs distribution. To use the mechanisms 
;;     provided in this file, load this file along with any dependencies in the init
;;     file with correct paths.

;; Code:

(defun jump-to-previous-end-of-block ()
  (interactive)
  "Moves point to the previous end of block immediately after the close brace."
  (let ((original_position) (position) (close_brace) (search) (start) (stop))
    (setq original_position (point))
    (setq position original_position)
    (setq start 1)
    (setq stop 0)
    (setq search start)
    
    (if (not (= position (point-min)))
	(if (string-equal "}" (char-to-string (char-before position)))
	    (setq position (- position 1))))

    (while (= search start)
      (setq close_brace (search-forward-character "}" position (point-min) -1))
      (if (not (equal close_brace nil))
	  (progn
	    (goto-char close_brace)
	    (if (= (check-if-within-comment) true)
		(setq position close_brace)
	      (setq search stop)))
	(setq search stop)))

    (if (not (equal close_brace nil))
	(goto-char (+ close_brace 1))
      (goto-char original_position))))
  

(defun jump-to-next-end-of-block ()
  (interactive)
  "Moves point to the next end of block immediately after the close brace."
  (let ((original_position) (position) (close_brace) (search) (start) (stop))
    (setq original_position (point))
    (setq position original_position)
    (setq start 1)
    (setq stop 0)
    (setq search start)
    
    (while (= search start)
      (setq close_brace (search-forward-character "}" position (point-max) 1))
      (if (not (equal close_brace nil))
	  (progn
	    (goto-char close_brace)
	    (if (= (check-if-within-comment) true)
		(setq position close_brace)
	      (setq search stop)))
	(setq search stop)))

    (if (not (equal close_brace nil))
	(goto-char close_brace)
      (goto-char original_position))))

(defun jump-to-previous-end-of-block-hook ()
  (define-key c-mode-base-map (kbd "C-,") 'jump-to-previous-end-of-block))

(defun jump-to-next-end-of-block-hook ()
  (define-key c-mode-base-map (kbd "C-.") 'jump-to-next-end-of-block))

(add-hook 'c-initialization-hook 'jump-to-previous-end-of-block-hook)
(add-hook 'c-initialization-hook 'jump-to-next-end-of-block-hook)
