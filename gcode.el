  ;; gcode.el --- Major mode for editing gcode
;; 
;; Author: Jeff Sapp <jasapp@gmail.com>
;;

(defvar gcode-mode-hook nil
  "*List of functions to call when entering GCode mode.*")

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
		(current-line-count 1))
	(goto-char (point-min))
	(while (re-search-forward "^[gmtGMT][0-9]\\{2\\}" nil t)
	  (beginning-of-line)
	  (insert (format "N%d " current-line-count))
	  (setq current-line-count (1+ current-line-count )))
	(goto-char original-point)))

(define-derived-mode gcode-mode fundamental-mode
  "Major mode for editing gcode."
  (setq font-lock-defaults '(gcode-font-lock-keywords))
  (setq major-mode 'gcode-mode)
  (setq mode-name "gcode")

  ;; modify the keymap
  (define-key gcode-mode-map [remap comment-dwim] 'gcode-comment-dwim)

  (modify-syntax-entry ?\( "< b" gcode-mode-syntax-table)
  (modify-syntax-entry ?\) "> b" gcode-mode-syntax-table)
  (run-hooks 'gcode-mode-hook))

(provide 'gcode)
