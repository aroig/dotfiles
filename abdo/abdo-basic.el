(provide 'abdo-basic)


;; Generic Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq system-time-locale "C")                 ;; POSIX locale for timestamps

(setq x-select-enable-clipboard t)            ;; Enable clipboard
(setq save-interprogram-paste-before-kill t)  ;; Saves the kill buffer
(setq mouse-drag-copy-region nil)             ;; Disable copy by mouse-drag

(transient-mark-mode t)                       ;; Some text highlighting
(show-paren-mode t)                           ;; Matching parenthesis

;; Default Fill column. This is overriden in latex or org-mode.
(setq-default fill-column 80)

(blink-cursor-mode t)                         ;; like it blinking

;; Enable auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Default major mode
(setq-default major-mode 'text-mode)

;; Set web browser to desktop default
 (setq browse-url-generic-program "xdg-open"
      browse-url-browser-function 'browse-url-generic)

;; Modeline tweaking:   http://www.emacswiki.org/emacs/ModeLineConfiguration
(setq display-time-format "%H:%M")
(display-time-mode 0)

(setq column-number-mode t)

;; All questions y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)


;; Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reload emacs config
(defun reload-emacs-config ()
  (interactive)
  (load-file "~/.emacs")
)

;; Perform a diff of current file.
;; If no prefix, performs diff of current buffer against file
;; If prefix, diffs two versions of the file in vcs
(defun abdo-ediff-current-file (arg)
  (interactive "P")
  (if (eq arg nil)
    (ediff-current-file)
    (ediff-revision (buffer-file-name))))


;; Performs a diff from command line parameters
(defun abdo-command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left))
	(file3 (pop command-line-args-left)))
    (if file3
	(ediff-files3 file1 file2 file3)
      (ediff-files file1 file2))))


;; Performs a merge
(defun abdo-command-line-merge (switch)
  (let ((file1 (pop command-line-args-left))    ; file A
	(file2 (pop command-line-args-left))    ; file B
	(file3 (pop command-line-args-left))    ; merge file
	(file4 (pop command-line-args-left)))   ; ancestor
    (if file4
	(ediff-merge-files-with-ancestor file1 file2 file4 nil file3)
      (ediff-merge-files file1 file2 nil file3))))



;; TODO keywords searching
(defvar fixme-keyword-re-string "FIXME\\|TODO\\|BUG"
  "The regular expression to use for searching for fixme words.")

(defun abdo-fixme-search ()
  "Show all fixme strings in the current file"
  (interactive)
  (let ((buf (buffer-file-name))
        (case-fold-search-old case-fold-search))
    (setq case-fold-search t)
    (when buf (occur fixme-keyword-re-string))
    (setq case-fold-search case-fold-search-old)
))



;; Launch terminal to current directory
(defun abdo-launch-terminal (arg)
  (interactive "P")
  (let ((path (expand-file-name default-directory)))
    (call-process "urxvt" nil 0 nil "-cd" path)))

;; Launch file manager on current directory
(defun abdo-launch-filemanager ()
  (interactive)
  (let ((path (expand-file-name default-directory)))

    ;; When there is a prefix, resolve symlinks
    ;; Seems it is not needed after all. Oh well.
;      (when (eq arg 4)
;      (setq path (file-truename (buffer-file-name)))
;      (string-match "\\(.*\\)/\\(.*\\)" path)
;      (setq path (match-string 1 path)))

    (call-process "nautilus" nil 0 nil path)))



;; Emacs backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Store backups on to abdo-emacs-backups for 5 days.

(when abdo-emacs-backups
  ;; Create backup dir if inexistent
  (make-directory abdo-emacs-backups t)

  (setq auto-save-file-name-transforms
   `((".*" ,abdo-emacs-backups t)))               ; store autosave files away

  (setq backup-directory-alist
    `((".*" . ,abdo-emacs-backups)))            ; store backups away

  ;; When using tramp with su or sudo, should store it for user root.
  (setq tramp-backup-directory-alist
        backup-directory-alist)                 ; also for tramp

  (setq backup-by-copying t)                    ; don't clobber symlinks
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq kept-old-versions 2)
  (setq version-control t)                      ; use versioned backups
  (setq make-backup-files t)                    ; make backup files
  (setq vc-make-backup-files t)                 ; also for files under vc

  ;; Cleanup backup directory
  (message "Deleting old backup files...")
  (let ((week (* 60 60 24 7))
        (current (float-time (current-time))))
    (dolist (file (directory-files abdo-emacs-backups t))
      (when (and (backup-file-name-p file)
                 (> (- current (float-time (fifth (file-attributes file))))
                    week))
        (message "  %s" file)
        (delete-file file))))
)


;; Disable backups for some files
(add-to-list 'auto-mode-alist '("\\.gpg$" . sensitive-mode))
(add-to-list 'auto-mode-alist `(,(file-truename "~/Private") . sensitive-mode))
(add-to-list 'auto-mode-alist `(,(file-truename "~/\\.ssh") . sensitive-mode))

(add-to-list 'auto-mode-alist '("\\.ido\\.last" . sensitive-mode))
(add-to-list 'auto-mode-alist '("\\.recentf" . sensitive-mode))



;; Emacs session handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface abdo-commit-question
  '((t :inherit default))
  "Face for commit question")

(defface abdo-save-question
  '((t :inherit default))
  "Face for save question")


(defun y-or-n-face-p (query face)
  (let (minibuffer-prompt-bak ans)
    (copy-face 'minibuffer-prompt 'minibuffer-prompt-bak)
    (unwind-protect
	(progn
          (copy-face face 'minibuffer-prompt)
	  (setq ans (y-or-n-p query)))
      (copy-face 'minibuffer-prompt-bak 'minibuffer-prompt))
    ans))


(defun abdo-path-contains-buffer (path)
  (delq nil (mapcar (lambda (buf)
                      (if (and (buffer-file-name buf) (string-prefix-p path (buffer-file-name buf)))
                          buf
                        nil))
                    (buffer-list))))


(defun abdo-ask-save-buffer (&optional buf)
  "asks if want to save buffer. Returns t if the file gets saved, and nil
 otherwise"
  (interactive)
  (setq buf (or buf (current-buffer)))
  (when (and (buffer-modified-p buf)
	     (buffer-file-name buf)
	     (y-or-n-face-p (concat "Save file " (buffer-file-name buf) "? ") 'abdo-save-question))
    (with-current-buffer buf (save-buffer)))
  (not (buffer-modified-p buf)))


(defun abdo-ask-commit-repo (&optional path)
  "Asks to commit the buffer. returns t if starting commit, and nil
   otherwise"
  (interactive)
  (setq path (or path (buffer-file-name)))

  (let ((rootdir (abdo-vc-root path)))
    (if (and rootdir (not (abdo-path-contains-buffer rootdir))) ; only ask for commit on the last buffer
	(if (y-or-n-face-p (concat "Commit changes to repo at " rootdir "? ") 'abdo-commit-question)
	    (progn (message "Preparing to commit") (abdo-vcs-status rootdir) t)
	  (message "Not commiting") nil)
      nil)))


;; This prevents emacs asking permission to kill a buffer that still has clients
(defun server-kill-buffer-query-function () t)


(defun abdo-client-visit-buffer (buf)
  "Registers the given buffer into the client, so it is killed when done"
  (let ((proc (frame-parameter (selected-frame) 'client)))
    (when (processp proc)
      (let ((clientbuf (process-get proc 'buffers)))
        (when (not (memq buf clientbuf))
          ; Why this? I don't remember, but it makes the client crash.
          ; (add-hook 'kill-buffer-hook 'server-kill-buffer nil t)
          (push proc server-buffer-clients)
          (process-put proc 'buffers (nconc (process-get proc 'buffers) `(,buf))))))))


(defun abdo-buffer-done ()
  "Marks a buffer as done in emacsclient, or kills in a standalone emacs
   session. Saves if needed."
  (interactive)
  (abdo-ask-save-buffer)
  (set-buffer-modified-p nil)
  (abdo~buffer-done-or-kill))


(defun abdo-buffer-done-commit ()
  "Marks a buffer as done in emacsclient, or kills in a standalone emacs
   session. Saves if needed and asks to commit if in a repo."
  (interactive)

  (when (not (and (abdo-ask-save-buffer)
		  (not (and buffer-file-name
			    (vc-registered buffer-file-name)
			    (vc-workfile-unchanged-p buffer-file-name)))
		  (abdo-ask-commit-repo buffer-file-name)))
    (set-buffer-modified-p nil)
    (abdo~buffer-done-or-kill)))


(defun abdo~buffer-done-or-kill ()
  (let ((proc (frame-parameter (selected-frame) 'client)))
    (if (and proc (process-get proc 'buffers))
        (server-edit)
    (save-buffers-kill-terminal))))


(defun abdo-commit-some-buffers (&optional arg pred)
  "Asks to to commit one repo belonging to currently open buffers. Similar to save-some-buffers.
   Here, a non-nil arg means do not commit without questioning."
  (interactive "P")

  ;; If arg, return without commiting anything!
  (unless arg
  (let (roothash rootlist rootdir val ret)

    ;; produce a hash table of different roots.
    ;; The value is the modified state. 0 modified, 1 in sync.
    (setq roothash (make-hash-table :test 'equal))
    (dolist (buf (buffer-list))
      (setq rootdir (abdo-vc-root (buffer-file-name buf)))
      (setq val (gethash rootdir roothash))
      (when (and rootdir
		 (not (equal val 0))
		 (buffer-live-p buf)
		 (not (buffer-base-buffer buf))
		 (buffer-file-name buf)
		 (not (and (vc-registered (buffer-file-name buf))
			   (vc-workfile-unchanged-p (buffer-file-name buf))))
		 (or (not (functionp pred)) (with-current-buffer buf (funcall pred))))

	(puthash rootdir (if (buffer-modified-p buf) 0 1) roothash)))

    (maphash (lambda (kk vv)
	       (when (equal vv 1) (setq rootlist (cons kk rootlist))))
	     roothash)

    ;; Start first commit and leave
    (setq ret nil)
    (while (and rootlist (not ret))
      (setq ret (abdo-ask-commit-repo (car rootlist)))
      (setq rootlist (cdr rootlist)))
    ret)))




(defun abdo-save-buffers-kill-terminal-commit (&optional arg)
  "Do a save-buffers-kill-terminal + ask to commit"
  (interactive "P")

  (let ((proc (frame-parameter (selected-frame) 'client))
	(commit nil))
    (cond ;; Nowait frames have no client buffer list.
          ((eq proc 'nowait)
	   (when (cdr (frame-list))
	     (save-some-buffers arg)
	     (setq commit (abdo-commit-some-buffers arg t))))

	  ((processp proc)
	   (let ((buffers (process-get proc 'buffers)))
	     ;; If client is bufferless, emulate a normal Emacs exit
	     ;; and offer to save all buffers.  Otherwise, offer to
	     ;; save only the buffers belonging to the client.

	     (save-some-buffers arg (if buffers (lambda () (memq (current-buffer) buffers)) t))
	     (setq commit (abdo-commit-some-buffers arg
		    (if buffers (lambda () (memq (current-buffer) buffers)) t)))))

	   ;; If not in client but a standalone session
          ((eq proc nil)
	   (save-some-buffers arg t)
	   (setq commit (abdo-commit-some-buffers arg t)))

	  ;; WTF
	  (t (error "Invalid client frame")))

    ;; If commited, don't kill emacs!
    (unless commit
      (cond ;; Nowait frames have no client buffer list.
          ((eq proc 'nowait)
	   (if (cdr (frame-list)) (delete-frame) (save-buffers-kill-emacs arg)))

	  ((processp proc)
	   (server-delete-client proc))

          ((eq proc nil)
           ;; Don't want emacs to ask again if saving, so make them all unmodified.
	   (dolist (buf (buffer-list)) (with-current-buffer buf (set-buffer-modified-p nil)))
	   (save-buffers-kill-emacs arg))

	  ;; WTF
	  (t (error "Invalid client frame"))))
    ))



;; outline magic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; when t, outline-cycle acts as tab on non-heading text
(setq outline-cycle-emulate-tab nil)

(add-hook 'outline-mode-hook
          (lambda ()
            (require 'outline-magic)))

(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)))



;; Version Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Follow symlinks
(setq vc-follow-symlinks t)


;; TODO: check if hg or git, and call the right function
(defun abdo-vcs-status (&optional rootdir)
  (interactive)
  ;; Default rootdir
  (unless rootdir (setq rootdir default-directory))
  (magit-status rootdir)
)


(defun abdo-responsible-backend (file)
  "Return the name of a backend system that is responsible for FILE.
   Returns nil if none is."

  (or (and (not (file-directory-p file)) (vc-backend file))
      (catch 'found
	;; First try: find a responsible backend.  If this is for registration,
	;; it must be a backend under which FILE is not yet registered.
	(dolist (backend vc-handled-backends)
	  (and (vc-call-backend backend 'responsible-p file)
	       (throw 'found backend)))
	(throw 'found nil))))


(defun abdo-vc-root (file)
  "Returns the root of the repo file belongs, or nil if file is not versioned."
  (when file
;;  (let ((backend (vc-backend file)))
    (let ((backend (abdo-responsible-backend file)))
      (when backend (vc-call-backend backend 'root file)))))


;; Speedbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sr-speedbar-right-side nil)



;; Window stuff: Winner, Windmove & Winring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/WinnerMode
;; http://www.emacswiki.org/emacs/WindMove
;; http://www.emacswiki.org/emacs/WinRing


;; Winring
; I use the window manager + emacs daemon now.

;(setq winring-keymap-prefix (kbd "H-w"))
;(require 'winring)
;(winring-initialize)
;(setq winring-show-names t)
;(setq winring-prompt-on-create nil)

;; Winner mode
(winner-mode 1)


;; Buffer Stuff: ido, ibuffer, uniquify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/InteractivelyDoThings

;; TODO: make ibuffer open on other window
;; TODO: combine ibuffer-vc and my tags
;;

;; Uniquify settings
(setq uniquify-buffer-name-style 'forward)

;; Launch ibuffer

(setq ibuffer-use-other-window t)
(setq ibuffer-display-summary nil)
(setq ibuffer-show-empty-filter-groups nil)

(defun abdo-ibuffer ()
    "Open ibuffer with cursour pointed to most recent buffer name"
    (interactive)
    (let ((recent-buffer-name (buffer-name)))
      (ibuffer)
      (ibuffer-jump-to-buffer recent-buffer-name)))


;; ibuffer settings
(setq abdo-ibuffer-filter
      (quote  (("Org" (mode . org-mode))
	       ("Mail"(or  (mode . message-mode) (mode . mail-mode)))
;              ("MyProject1" (filename . "src/myproject1/"))
	       )))

;; Formats
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
	      (name 18 18 :left :elide) " "
	      (size 9 -1 :right) " "
	      (mode 16 16 :left :elide) " "
	      (vc-status 16 16 :left) " "
	      filename-and-process)))


(add-hook 'ibuffer-mode-hook (lambda ()
			       (setq ibuffer-saved-filter-groups
				     (list (append '("default") abdo-ibuffer-filter
					   (ibuffer-vc-generate-filter-groups-by-vc-root))))

			       (ibuffer-switch-to-saved-filter-groups "default")
			       (ibuffer-do-sort-by-alphabetic)))

;; Ido config
;; WARNING: This is not compatible with icicles !

(ido-mode t)
(ido-everywhere t)
(setq ido-max-prospects 8)
(setq ido-rotate t)
(setq ido-enable-flex-matching t)
(setq ido-enable-regexp t)
;; (setq ido-use-filename-at-point t)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-default-buffer-method 'selected-window)


;; Undo Tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/UndoTree

;; It seems that evil decides whether to load undo-tree or not ... despite this.
;; Undo-tree does not load for fundamental mode.
(global-undo-tree-mode t)



;; Ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/EdiffMode

;; Single frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Split either vertically or horizontally. Depending on frame size
(setq ediff-split-window-function (if (> (frame-width) 150)
                                          'split-window-horizontally
                                        'split-window-vertically))

;; Hooks to save and restore window configuration
(add-hook 'ediff-load-hook
	  (lambda ()
	    (add-hook 'ediff-before-setup-hook
		      (lambda ()
			(setq ediff-saved-window-configuration (current-window-configuration))))

	    (let ((restore-window-configuration
		   (lambda ()
		     (set-window-configuration ediff-saved-window-configuration))))
	      (add-hook 'ediff-quit-hook restore-window-configuration 'append)
	      (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))


;; recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/RecentFiles

(setq recentf-auto-cleanup 'never)  ;; need it because of tramp
(recentf-mode 1)
(setq recentf-max-saved-items 500)



;; Autocomplete:        http://www.emacswiki.org/emacs/AutoComplete
;; Disabled. Made me nervous

;; (ac-config-default)
;; (setq ac-auto-start nil)     ; Do not automatically auto-complete
;; (ac-set-trigger-key "TAB")   ; Set trigger key


;; Emacs 1on1. Disabled because seems quite unmantained
;; (setq 1on1-minibuffer-frame-flag nil)
;; (require 'oneonone)
;; (1on1-emacs)
