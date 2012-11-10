
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-directory-files-recursive (directory &optional regexp)
  "List the .el files in DIRECTORY and in its sub-directories."
  (interactive "DDirectory name: ")
  (let (el-files-list
	(current-directory-list
	 (directory-files-and-attributes directory t)))
    (while current-directory-list
      (cond
       ((string-match regexp (car (car current-directory-list)))
	(setq el-files-list
	      (cons (car (car current-directory-list)) el-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))

	;; decide whether to skip or recurse
	(if
	    (equal "." (substring (car (car current-directory-list)) -1))
	    ;; then do nothing since filename is that of
	    ;;   current directory or parent, "." or ".."
	    ()
	  ;; else descend into the directory and repeat the process
	  (setq el-files-list
		(append
		 (abdo-directory-files-recursive
		  (car (car current-directory-list)) regexp)
		 el-files-list)))))

      (setq current-directory-list (cdr current-directory-list)))
    el-files-list))


;; Helm sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Highlighting stuff
(defun abdo-helm-c-highlight-org-files (files source)
  (loop for i in files
        collect (propertize (file-relative-name i org-directory)
                            'face 'helm-ff-file
                            'help-echo (expand-file-name i))))


; Org mode sources
(defvar helm-c-source-org-files
  `((name . "Org files")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               "*org files*"
               (abdo-directory-files-recursive org-directory-wiki ".*\\.org$"))))
    (candidates-in-buffer)
    ;; TODO: Why the following fails?
;    (filtered-candidate-transformer abdo-helm-c-highlight-org-files)
    (match helm-c-match-on-basename)
    (type . file)))


;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-helm-org-files ()
  (interactive)
  (helm-other-buffer '(helm-c-source-org-files)
                     "*helm org*"))

(defun abdo-helm-todos ()
  (interactive)
  (helm-other-buffer '(helm-c-source-fixme)
                     "*helm org*"))


(provide 'abdo-helm)
