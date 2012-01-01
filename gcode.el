;; gcode.el --- Major mode for editing gcode
;; 
;; Author: Jeff Sapp <jasapp@gmail.com>
;;
(require 'newcomment)
(require 'cl)

(defvar gcode-mode-hook nil
  "*List of functions to call when entering GCode mode.*")

(defvar gcode-font-lock-keywords
  (list '("\\<\\([gmtGMT][0-9]\\{1,4\\}\\)\\>" . font-lock-function-name-face)
	'("\\<\\(^[nN][0-9]+\\)\\>" . font-lock-type-face)
	'("\\<\\([A-Za-z][+-]?[.]?[0-9]+\\(\\.[0-9]+\\)?\\)\\>" . font-lock-keyword-face)))

(defvar args
  (list '(G00 (x y z a b c))
	'(G01 (x y z a b c))
	'(G02 (x y z a b c r))
	'(G03 (x y z a b c r))))

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
	(while (re-search-forward "^[gmtGMT][0-9]\\{1,4\\}" nil t)
	  (beginning-of-line)
	  (insert (format "N%d " current-line-count))
	  (setq current-line-count (+ current-line-count 5)))
	(goto-char original-point)))

(defun update-line-numbers () 
  (interactive "*")
  (remove-line-numbers)
  (add-line-numbers))

(defun gcode-commandp (str)
  (string-match "\\<\\([gmtGMT][0-9]\\{1,4\\}\\)\\>" str))

(defun gcode-argp (str)
  (not (gcode-commandp str)))

(defun list-line-commands ()
  (interactive "*")
  (let ((line (buffer-substring-no-properties (line-beginning-position)
					      (line-end-position))))
    (remove-if-not 'gcode-commandp (split-string line))))

(defun display-available-arguments ()
  "Display arguments for the commands the commands on the current line."
  (interactive "*")
  (let ((current-commands (list-line-commands)))
    (mapconcat 'identity current-commands " ")))

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
