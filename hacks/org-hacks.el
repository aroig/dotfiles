
;; org-mobile hacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make target filenames from the symlink instead of the true filename

(defun org-mobile-files-alist ()
  "Expand the list in `org-mobile-files' to a list of existing files.
Also exclude files matching `org-mobile-files-exclude-regexp'."
  (let* ((include-archives
	  (and (member 'org-agenda-text-search-extra-files org-mobile-files)
	       (member 'agenda-archives	org-agenda-text-search-extra-files)
	       t))
	 (files
	  (apply 'append
		 (mapcar
		  (lambda (f)
		    (cond
		     ((eq f 'org-agenda-files)
		      (org-agenda-files	t include-archives))
		     ((eq f 'org-agenda-text-search-extra-files)
		      (delq 'agenda-archives
			    (copy-sequence
			     org-agenda-text-search-extra-files)))
		     ((and (stringp f) (file-directory-p f))
		      (directory-files f 'full "\\.org\\'"))
		     ((and (stringp f) (file-exists-p f))
		      (list f))
		     (t nil)))
		  org-mobile-files)))
	 (files (delete
		 nil
		 (mapcar (lambda (f)
			   (unless (and (not (string= org-mobile-files-exclude-regexp ""))
					(string-match org-mobile-files-exclude-regexp f))
			     (identity f)))
			 files)))
	 (orgdir-uname (file-name-as-directory (file-truename org-directory)))
	 (orgdir-re (concat "\\`" (regexp-quote orgdir-uname)))
	 uname seen rtn file link-name)
    ;; Make the files unique, and determine the name under which they will
    ;; be listed.
    (while (setq file (pop files))
      (if (not (file-name-absolute-p file))
	  (setq file (expand-file-name file org-directory)))
      (setq uname (file-truename file))
      (unless (member uname seen)
	(push uname seen)
	(if (string-match orgdir-re file)
	    (setq link-name (substring file (match-end 0)))
	  (setq link-name (file-name-nondirectory file)))
	(push (cons file link-name) rtn)))
    (nreverse rtn)))


(provide 'org-hacks)
