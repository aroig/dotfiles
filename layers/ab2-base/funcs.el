;; Faces, colors, etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ab2/what-face (pos)
  "Return the face at pos"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defun ab2/high-colors-p ()
  "Check if we are in a terminal with <= 16 colors available"
  (or (display-graphic-p) (> (length (tty-color-alist)) 16)))



;; String manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ab2/strip (str)
  "Chomp leading and tailing whitespace from STR."
  (when str
	(let ((tmpstr str))
	  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"  tmpstr)
	    (setq tmpstr (replace-match "" t t tmpstr)))
	  tmpstr)))



;; Escape a string into a regexp that matches it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ab2/escape-regexp-repl (s)
  (cond
    ((string= s "\\") "\\\\\\\\")
    ((string= s ".") "\\\\.")
    ((string= s "*") "\\\\*")
    ((string= s "+") "\\\\+")
    ((string= s "?") "\\\\?")
    ((string= s "[") "\\\\[")
    ((string= s "^") "\\\\^")
    ((string= s "$") "\\\\$")))

(defun ab2/escape-regexp(str)
  "Escapes str to get a regular expression that matches it"
  (setq str (replace-regexp-in-string "\\\\" 'abdo-escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\." 'abdo-escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\*" 'abdo-escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\+" 'abdo-escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\?" 'abdo-escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\[" 'abdo-escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\^" 'abdo-escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\^" 'abdo-escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\$" 'abdo-escape-regexp-repl str)))
