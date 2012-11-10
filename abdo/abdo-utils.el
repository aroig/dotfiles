
(provide 'abdo-utils)

(defun abdo-utils-strip (str)
      "Chomp leading and tailing whitespace from STR."
      (when str
	(let ((tmpstr str))
	  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"  tmpstr)
	    (setq tmpstr (replace-match "" t t tmpstr)))
	  tmpstr)
      ))


(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))