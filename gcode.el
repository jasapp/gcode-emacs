;; gcode.el --- Major mode for editing gcode
;; 
;; Author: Jeff Sapp <jasapp@gmail.com>
;;
;; add feed rate changes everywhere. 
;; change depth of cut
;; speeds
;; quick change of canned cycles, G82,83,84
(require 'newcomment)

(defvar gcode-mode-hook nil
  "*List of functions to call when entering GCode mode.*")

(defvar gcode-font-lock-keywords
  (list '("\\<\\([gmtGMT][0-9]\\{2\\}\\)\\>" . font-lock-function-name-face)
		'("\\<\\(^[nN][0-9]+\\)\\>" . font-lock-type-face)
		'("\\<\\([A-Za-z][+-]?[0-9]+\\(\\.[0-9]+\\)?\\)\\>" . font-lock-keyword-face)
		))

(defun G00 () 
  "Rapid positioning.

On 2- or 3-axis moves, G00 (unlike G01) does not necessarily move in a 
single straight line between start point and end point. It moves each 
axis at its max speed until its vector is achieved. Shorter vector 
usually finishes first (given similar axis speeds).")

(defun G01 () "Linear interpolation")
(defun G02 () "Circular interpolation, clockwise.")


(defun remove-line-numbers ()
  "Remove line numbers."
  (interactive "*")
  (let ((original-point (point)))
	(goto-char (point-min))
	(while (re-search-forward "^[nN][0-9]+[ \n\t]" nil t)
	  (replace-match ""))
	(goto-char original-point)))

(defun add-line-numbers ( )
  "Add line numbers."
  (interactive "*")
  (remove-line-numbers)
  (let ((original-point (point))
		(current-line-count 0))
	(goto-char (point-min))
	(while (re-search-forward "^[gmtGMT][0-9]\\{2\\}" nil t)
	  (beginning-of-line)
	  (insert (format "N%d " current-line-count))
	  (setq current-line-count (+ current-line-count 5)))
	(goto-char original-point)))

(defun update-line-numbers () 
  (interactive "*")
  (remove-line-numbers)
  (add-line-numbers))

(define-derived-mode gcode-mode fundamental-mode
  "Major mode for editing gcode."
  (setq font-lock-defaults '(gcode-font-lock-keywords))
  (setq major-mode 'gcode-mode)
  (setq mode-name "gcode")

  (setq comment-start "(") 
  (setq comment-end ")")

  (modify-syntax-entry ?\( "< b" gcode-mode-syntax-table)
  (modify-syntax-entry ?\) "> b" gcode-mode-syntax-table)
  (run-hooks 'gcode-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gc$" . gc-mode))

(provide 'gcode)
