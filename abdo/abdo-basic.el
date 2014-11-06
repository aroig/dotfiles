
;; Generic Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq system-time-locale "C")                 ;; POSIX locale for timestamps
(setq calendar-week-start-day 1)              ;; Start calendar on monday

(setq x-select-enable-clipboard t)            ;; Enable clipboard
(setq save-interprogram-paste-before-kill t)  ;; Saves the kill buffer
(setq mouse-drag-copy-region nil)             ;; Disable copy by mouse-drag

(transient-mark-mode t)                       ;; Some text highlighting
(show-paren-mode t)                           ;; Matching parenthesis

(put 'narrow-to-region 'disabled nil)         ;; Enable narrowing
(setq-default fill-column 90)                 ;; Default Fill column.
(blink-cursor-mode t)                         ;; like it blinking


(add-hook 'text-mode-hook 'turn-on-auto-fill) ;; Enable auto-fill

(setq-default major-mode 'text-mode)          ;; Default major mode

(setq bookmark-save-flag 1)                   ;; save bookmarks immediately

(setq server-raise-frame nil)                 ;; don't raise frames when switching buffers

(defalias 'yes-or-no-p 'y-or-n-p)             ;; All questions y-or-n

;; Set web browser to desktop default
 (setq browse-url-generic-program "xdg-open"
      browse-url-browser-function 'browse-url-generic)

;; Modeline tweaking:   http://www.emacswiki.org/emacs/ModeLineConfiguration
(setq display-time-format "%H:%M")            ;; time format
(display-time-mode 0)                         ;; hide time
(setq column-number-mode t)                   ;; display column number

;; Since I use git outside of emacs, do not rely on magit-auto-revert-mode.
;; I instead I use my own-auto-revert
(setq magit-auto-revert-mode nil)



;; Emacs backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Store backups on to abdo-emacs-backups for 5 days.

(when abdo-emacs-backups
  ;; Create backup dir if inexistent
  (make-directory abdo-emacs-backups t)

  (setq auto-save-file-name-transforms
   `((".*" ,abdo-emacs-backups t)))             ; store autosave files away

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
)



;; Version Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Follow symlinks
(setq vc-follow-symlinks t)

;; TODO: check if hg or git, and call the right function
(defun abdo-vcs-status (&optional rootdir)
  (interactive)
  ;; Default rootdir
  (unless rootdir (setq rootdir default-directory))
  (magit-status rootdir))

;; TODO: check if hg or git, and call the right function
(defun abdo-vcs-branches (&optional rootdir)
  (interactive)
  ;; Default rootdir
  (unless rootdir (setq rootdir default-directory))
  (magit-branch-manager))

;; TODO: check if hg or git, and call the right function
(defun abdo-vcs-log (&optional rootdir)
  (interactive)
  ;; Default rootdir
  (unless rootdir (setq rootdir default-directory))
  (magit-log))

(defun abdo-vcs-root (file)
  "Returns the root of the repo file belongs, or nil if file is not versioned."
  (when (and file (vc-backend file)) (file-truename (vc-call root file))))



;; monkeypatch vc-mode-line in vc-hooks.el so I get a lowercase modeline string.
(defun vc-mode-line (file &optional backend)
  "Set `vc-mode' to display type of version control for FILE.
The value is set in the current buffer, which should be the buffer
visiting FILE.
If BACKEND is passed use it as the VC backend when computing the result."
  (interactive (list buffer-file-name))
  (setq backend (or backend (vc-backend file)))
  (if (not backend)
      (setq vc-mode nil)
    (let* ((ml-string (vc-call-backend backend 'mode-line-string file))
	   (ml-echo (get-text-property 0 'help-echo ml-string)))
      (setq vc-mode
            (if (null vc-display-status)
                (downcase (symbol-name backend))
              (propertize
               (downcase ml-string)
               'mouse-face 'mode-line-highlight
               'help-echo
               (concat (or ml-echo
                           (format "File under the %s version control system"
                                   backend))
                       "\nmouse-1: Version Control menu")
               'local-map vc-mode-line-map))))
    ;; If the user is root, and the file is not owner-writable,
    ;; then pretend that we can't write it
    ;; even though we can (because root can write anything).
    ;; This way, even root cannot modify a file that isn't locked.
    (and (equal file buffer-file-name)
	 (not buffer-read-only)
	 (zerop (user-real-uid))
	 (zerop (logand (file-modes buffer-file-name) 128))
	 (setq buffer-read-only t)))
  (force-mode-line-update)
  backend)



(provide 'abdo-basic)
