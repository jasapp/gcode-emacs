;; gcode.el --- Major mode for editing gcode
;;
;; Author: Jeff Sapp <jasapp@gmail.com>
;;

(require 'newcomment)
(require 'cl)

(defvar gcode-mode-hook nil
  "List of functions to call when entering GCode mode.")

(defvar gcode-font-lock-keywords
  (list '("\\<\\([gmtGMT][0-9]\\{1,4\\}\\)\\>" . font-lock-function-name-face)
        '("\\<\\(^[nN][0-9]+\\)\\>" . font-lock-type-face)
        '("\\<\\([A-Za-z][+-]?[.]?[0-9]+\\(\\.[0-9]+\\)?\\)\\>" . font-lock-keyword-face)))

(defvar gcode-args
  (list '("G0" ("X" "Y" "Z" "A" "B" "C" "E"))
        '("G1" ("X" "Y" "Z" "A" "B" "C" "E"))
        '("G2" ("X" "Y" "Z" "A" "B" "C" "E" "I" "J" "R"))
        '("G3" ("X" "Y" "Z" "A" "B" "C" "E" "I" "J" "R"))))

(defun args (fn-str)
  (second (assoc fn-str gcode-args)))

(defun clean-returned-file ()
  "Remove control characters returned from machine."
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
  (setq-local font-lock-defaults '(gcode-font-lock-keywords))
  (setq-local major-mode 'gcode-mode)
  (setq-local mode-name "gcode")

  (setq-local comment-start "; ")
  (setq-local comment-end "")

  (setq-local gcode-mode-map (make-sparse-keymap))
  ;;  (define-key gcode-mode-map (kbd "SPC") 'new-space)

  (modify-syntax-entry ?\; "<" gcode-mode-syntax-table)
  (modify-syntax-entry ?\n ">" gcode-mode-syntax-table)
  (modify-syntax-entry ?\( "< b" gcode-mode-syntax-table)
  (modify-syntax-entry ?\) "> b" gcode-mode-syntax-table)
  (run-hooks 'gcode-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gc\\(ode\\)?$" . gcode-mode))
(add-to-list 'auto-mode-alist '("\\.GC\\(ODE\\)?$" . gcode-mode))
(add-to-list 'auto-mode-alist '("\\.nc)?$" . gcode-mode))
(add-to-list 'auto-mode-alist '("\\.NC)?$" . gcode-mode))
(add-to-list 'auto-mode-alist '("\\.ngc)?$" . gcode-mode))
(add-to-list 'auto-mode-alist '("\\.NGC)?$" . gcode-mode))

(provide 'gcode)
