  ;; gcode.el --- Major mode for editing gcode
;; 
;; Author: Jeff Sapp <jasapp@gmail.com>
;;

(defvar gcode-mode-hook nil
  "*List of functions to call when entering GCode mode.*")

(defvar gcode-mode-map nil
  "Keymap for major mode.")


(defvar gcode-font-lock-keywords
  (list '("\\<\\([gmtGMT][0-9]\\{2\\}\\)\\>" . font-lock-function-name-face)
		'("\\<\\(^[nN][0-9]+\\)\\>" . font-lock-type-face)
		'("\\<\\([A-Z][+-]?[0-9]+\\(\\.[0-9]+\\)?\\)\\>" . font-lock-keyword-face)
		))

(defun gcode-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) 
	 (comment-start "(") 
	 (comment-end ")"))
     (comment-dwim arg)))

(defun remove-line-numbers ()
  "Remove line numbers"
  (interactive "*")
  (let ((original-point (point)))
	(goto-char (point-min))
	(while (re-search-forward "^[nN][0-9]+[ \n\t]+" nil t)
	  (replace-match ""))
	(goto-char original-point)))

(defun gcode-mode () 
  "Major mode for editing gcode."
  (interactive)
  (kill-all-local-variables)
  (text-mode)
  (setq major-mode 'gcode-mode)
  (setq mode-name "Gcode")
  (set (make-local-variable 'font-lock-defaults) 
       '(gcode-font-lock-keywords))
  (run-hooks 'gcode-mode-hook))

(provide 'gcode)
