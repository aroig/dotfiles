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



;; Projects and layouts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: projectile does not declare this as autoload
(autoload 'projectile-switch-project-by-name "projectile")
(autoload 'projectile-root-bottom-up "projectile")

(defun ab2/find-file-in-project (filename)
  "Open file in a project."
  (interactive "P")
  (let* ((persp-reset-windows-on-nil-window-conf t)
         (projectile-switch-project-action (lambda ()))
         (path (file-truename filename))
         (root (projectile-root-bottom-up (if (file-directory-p path)
                                              (file-name-as-directory path)
                                            (file-name-directory path)))))
    (when root
      (let ((project (file-name-nondirectory (directory-file-name root))))
            (persp-switch project)
            (projectile-switch-project-by-name root)
            (find-file path)))))



(defun ab2/frame-layout (name)
  "Start a custom layout attached to a frame. When the frame is killed, so is the layout."
  (interactive "P")
  (lexical-let ((layout name))
    (funcall (spacemacs//custom-layout-func-name name))
    (make-variable-frame-local 'delete-frame-functions)
    (add-hook 'delete-frame-functions (lambda (arg) (persp-kill layout)))))



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
  (setq str (replace-regexp-in-string "\\\\" 'ab2/escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\."  'ab2/escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\*"  'ab2/escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\+"  'ab2/escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\?"  'ab2/escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\["  'ab2/escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\^"  'ab2/escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\^"  'ab2/escape-regexp-repl str))
  (setq str (replace-regexp-in-string "\\$"  'ab2/escape-regexp-repl str)))
