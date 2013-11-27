
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-directory-files-rec (directory &optional regexp)
  "List the files matching regexp in directory and its subdirectories."
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
		 (abdo-directory-files-rec
		  (car (car current-directory-list)) regexp)
		 el-files-list)))))

      (setq current-directory-list (cdr current-directory-list)))
    el-files-list))


;; Generic helm sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; files in preconfigured dir
(defun helm-c-source-files-in-dir-rec (path regexp name)
  `((name . ,name)
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (when (file-accessible-directory-p ,path)
                          (abdo-directory-files-rec ,path ,regexp)))))
;    (keymap . ,helm-generic-files-map)
    (no-delay-on-input)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (type . file)))




;; org mode helm sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar helm-c-source-org-files (helm-c-source-files-in-dir-rec
                                 org-directory-wiki
                                 ".*\\.org$"
                                 "Org files"))


;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-helm-org-files ()
  (interactive)
  (helm-other-buffer 'helm-c-source-org-files "*helm org files*"))


(defun abdo-helm-todos ()
  (interactive)
  (helm-other-buffer '(helm-c-source-fixme)
                     "*helm TODO*"))



;; Helm hacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do not touch my mode-line!
;; Let's monkey-patch the modeline setup in helm.
(defun helm-display-mode-line (source)
  "Setup mode-line and header-line for `helm-buffer'."
  (set (make-local-variable 'helm-mode-line-string)
       (helm-interpret-value (or (and (listp source) ; Check if source is empty.
                                      (assoc-default 'mode-line source))
                                 (default-value 'helm-mode-line-string))
                             source))

  (let ((follow (and (eq (cdr (assq 'follow source)) 1) "(HF) ")))
    ;; Setup mode-line.
    (setq mode-line-process
          (format " %s" (helm-show-candidate-number
                         (when (listp helm-mode-line-string)
                           (car-safe helm-mode-line-string)))))

    ;; Setup header-line.
    (let* ((hlstr (helm-interpret-value
                   (and (listp source)
                        (assoc-default 'header-line source)) source))
           (hlend (make-string (max 0 (- (window-width) (length hlstr))) ? )))
      (setq header-line-format
            (propertize (concat " " hlstr hlend) 'face 'helm-header)))))


(provide 'abdo-helm)
