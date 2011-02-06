;; gcode.el --- Major mode for editing gcode
;; 
;; Author: Jeff Sapp <jasapp@gmail.com>
;;

(defconst gcode--gcode-re "[GM][0-9][0-9]")

(defvar gcode-mode-hook nil
  "*List of functions to call when entering GCode mode.*")

(defvar gcode-mode-map nil
  "Keymap for major mode.")


(defvar gcode-font-lock-keywords
  (list '("\\<\\([gmGM][0-9]\\{2\\}\\)\\>" . font-lock-function-name-face)))

(defun gcode-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) 
	 (comment-start "(") 
	 (comment-end ")"))
     (comment-dwim arg)))

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
