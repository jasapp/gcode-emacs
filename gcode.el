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

(defvar gcode-args
  (list '("G0" ("X" "Y" "Z" "A" "B" "C"))
	'("G1" ("X" "Y" "Z" "A" "B" "C"))
	'("G2" ("X" "Y" "Z" "A" "B" "C" "R"))
	'("G3" ("X" "Y" "Z" "A" "B" "C" "R"))))

(defun args (fn-str)
  (second (assoc fn-str gcode-args)))

(defun G00 () 
  "Rapid positioning.

On 2- or 3-axis moves, G00 (unlike G01) does not necessarily move in a 
single straight line between start point and end point. It moves each 
axis at its max speed until its vector is achieved. Shorter vector 
usually finishes first (given similar axis speeds).")

(defun G01 () "Linear interpolation")
(defun G02 () "Circular interpolation, clockwise.")

(defun clean-returned-file ()
  "Remove control characters returned from mach."
  (interactive "*")
  (let ((original-point (point)))
    (goto-char (point-min))
    (while (re-search-forward "[\|\|\]" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "\\([0-9]\\)\\([A-Za-z]\\)" nil t)
      (replace-match "\\1 \\2"))
    (goto-char original-point)))

(defun gcode-commandp (str)
  (string-match "\\<\\([gmtGMT][0-9]\\{1,4\\}\\)\\>" str))

(defun gcode-argp (str)
  (not (gcode-commandp str)))

(defun split-gcode (str)
  (let ((code-chars (mapcar 'string str)))
    (cons (first code-chars)
	  (mapconcat 'identity (rest code-chars) ""))))

(defun list-line-args ()
  (interactive "*")
  (let* ((line (buffer-substring-no-properties (line-beginning-position)
					       (line-end-position)))
	 (line-args (remove-if 'gcode-commandp (split-string line))))
    line-args))

(defun list-line-addresses ()
  (mapcar '(lambda (x) (first (split-gcode x))) (list-line-args)))

(defun list-line-commands ()
  (interactive "*")
  (let ((line (buffer-substring-no-properties (line-beginning-position)
					       (line-end-position))))
    (remove-if-not 'gcode-commandp (split-string line))))

(defun available-arguments ()
  "Display arguments for the commands the commands on the current line."
  (let* ((cmds (list-line-commands))
	 (available (apply 'append (mapcar 'args cmds)))
	 (current (list-line-addresses)))
    (remove-if (lambda (x) (member x current)) available)))

(defun display-available-arguments () 
  (interactive "*")
  (message (mapconcat 'identity (available-arguments) " ")))

(defun new-space ()
  (insert " " )
  (display-available-arguments))

(define-derived-mode gcode-mode fundamental-mode
  "Major mode for editing gcode."
  (setq font-lock-defaults '(gcode-font-lock-keywords))
  (setq major-mode 'gcode-mode)
  (setq mode-name "gcode")

  (setq comment-start "(") 
  (setq comment-end ")")

  (setq gcode-mode-map (make-sparse-keymap))
;;  (define-key gcode-mode-map (kbd "SPC") 'new-space)

  (modify-syntax-entry ?\( "< b" gcode-mode-syntax-table)
  (modify-syntax-entry ?\) "> b" gcode-mode-syntax-table)
  (run-hooks 'gcode-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gc\\(ode\\)?$" . gcode-mode))
(add-to-list 'auto-mode-alist '("\\.NC)?$" . gcode-mode))

(provide 'gcode)
