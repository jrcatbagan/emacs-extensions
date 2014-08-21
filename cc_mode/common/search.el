;; File: search.el
;; Created: 15, December 2013

;; Copyright (C) Jarielle Catbagan
;;
;; BSD License
;;
;; Please refer to LICENSE.txt for license details

;; Dependencies:

;; Description:

;; Notes:

;;  1. This file is not part of the GNU Emacs distribution. To use the mechanisms 
;;     provided in this file, load this file along with any dependencies in the init
;;     file with correct paths.

;; Code:

(defun search-forward-character (character start end &optional occurrence)
  "Searches for the occurrence of the character within the specified range. If the 
character is found, the point after the character is returned. On the contrary, if
the character is not found, nil is returned instead. Character must be a string
literal."
  (save-excursion
    (goto-char start)
    (search-forward character end 0 occurrence)))

(defun search-forward-string (string start end &optional occurrence)
  "Searches for the occurence of the string within the specified range. If the
string is found, the point after the string is returned. On the contrary, if the
character is not found, nil is returned instead."
  (save-excursion
    (goto-char start)
    (search-forward string end 0 occurrence)))
