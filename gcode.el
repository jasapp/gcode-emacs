;; gcode.el --- Major mode for editing gcode
;; 
;; Author: Jeff Sapp <jasapp@gmail.com>
;;

(defvar gcode-mode-hook nil
  "*List of functions to call when entering gcode mode.*")

(defvar gcode-mode-map (make-keymap))

(defvar gcode-font-lock-keywords
  (list '("\\<\\([gmtGMT][0-9]\\{2\\}\\)\\>" . font-lock-function-name-face)
		'("\\<\\([nN][0-9]+\\)\\>" . font-lock-type-face)
		'("\\<\\([A-Z][+-]?[0-9]+\\(\\.[0-9]+\\)?\\)\\>" . font-lock-keyword-face)
		))

(defun xx-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
     (comment-dwim arg)))

(define-derived-mode gcode-mode fundamental-mode
  "Major mode for editing gcode."
  ;; (interactive)
  ;; (kill-all-local-variables)
  (setq major-mode 'gcode-mode)
  (setq mode-name "Gcode")

  ;; setup our fonts
  (set (make-local-variable 'font-lock-defaults) 
       '(gcode-font-lock-keywords))

  ;; setup our comments
  (define-key gcode-mode-map [remap comment-dwim] 'xx-comment-dwim)
  (modify-syntax-entry ?# "< b" xx-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" xx-mode-syntax-table)

  (run-hooks 'gcode-mode-hook))

(provide 'gcode)
