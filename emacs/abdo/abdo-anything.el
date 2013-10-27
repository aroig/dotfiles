


(defun abdo-anything-things ()
  (require 'anything-config)
  (require 'anything-match-plugin)
  (require 'anything-hacks)
)


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





;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://emacs-fu.blogspot.com/2011/09/finding-just-about-anything.html

(defun abdo-anything-find-file ()
  (interactive)
  (abdo-anything-things)
  (anything
     :prompt "Switch to: "
     :candidate-number-limit 20                 ;; up to 10 of each 
     :sources
     '( anything-c-source-buffers               ;; buffers 
        anything-c-source-recentf               ;; recent files 
        anything-c-source-bookmarks             ;; bookmarks
        anything-c-source-files-in-current-dir+ ;; current dir
;        anything-c-source-locate                ;; use 'locate'
     )))          

(defun abdo-anything-org-find-file ()
  (interactive)
  (abdo-anything-things)
  (anything
     :prompt "Switch to: "
     :candidate-number-limit 20                 ;; up to 10 of each 
     :sources
     '( anything-c-source-files-in-org-dir+ )))


(defun abdo-anything-org-headline ()
  (interactive)
  (abdo-anything-things)
  (anything
     :prompt "Switch to: "
     :candidate-number-limit 20                 ;; up to 10 of each 
     :sources
     '( abdo-anything-c-source-org-headline )))


(provide 'abdo-anything)

