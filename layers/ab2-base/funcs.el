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
  "Open a file like find-file. If the file belongs to a project, creates
   a new persp and enables projectile mode for it."
  (interactive "P")
  ;; need the require since the projectile functions used here are not auto-loadable.
  (require 'projectile)
  (let* ((persp-reset-windows-on-nil-window-conf t)
         (filename-fullpath (file-truename filename))
         (filename-directory (if (file-directory-p filename-fullpath)
                                 (file-name-as-directory filename-fullpath)
                               (file-name-directory filename-fullpath)))
         (projectile-switch-project-action (lambda () (find-file filename-fullpath)))
         (project-root (projectile-root-bottom-up filename-directory)))
    (if project-root
        (progn
          (persp-switch (file-name-nondirectory (directory-file-name project-root)))
          (projectile-switch-project-by-name project-root))
      (message "Requested file does not belong to any project"))))

(defun ab2/frame-kill-layout (frame)
  (let ((layout (frame-parameter frame 'ab2/frame-layout)))
    (when layout (persp-kill layout))))

(add-hook 'delete-frame-functions 'ab2/frame-kill-layout)

(defun ab2/frame-layout (name)
  "Start a custom layout attached to a frame. When the frame is killed, so is the layout."
  (interactive "P")
  (funcall (spacemacs//custom-layout-func-name name))
  (modify-frame-parameters (selected-frame) `((ab2/frame-layout . ,name))))



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
