;; File: point.el
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

(defun beginning-of-line-point ()
  "Returns the position at the beginning of the current line."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun end-of-line-point ()
  "Returns the position at the end of the current line."
  (save-excursion
    (end-of-line)
    (point)))
