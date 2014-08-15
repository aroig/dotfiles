
;; Emacs session handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface abdo-commit-question '((t :inherit default))
  "Face for commit question")

(defface abdo-save-question '((t :inherit default))
  "Face for save question")

(defvar abdo-commit-on-kill t
  "If non-nil asks to commit on kill")

(defvar abdo-save-buffers-new-frame-kill-processes nil)



(defun y-or-n-face-p (query face)
  (let (minibuffer-prompt-bak ans)
    (copy-face 'minibuffer-prompt 'minibuffer-prompt-bak)
    (unwind-protect
	(progn
          (copy-face face 'minibuffer-prompt)
	  (setq ans (y-or-n-p query)))
      (copy-face 'minibuffer-prompt-bak 'minibuffer-prompt))
    ans))


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

  (let ((rootdir (abdo-vcs-root path))
        (buflist '()))

    ;; get a list of buffers which open files inside rootdir and have changes
    (when rootdir
      (setq buflist (mapcar (lambda (buf)
                              (if (and (not (eq buf (current-buffer)))
                                       (buffer-file-name buf)
                                       (string-prefix-p rootdir (buffer-file-name buf))
                                       (not (eq (vc-state (buffer-file-name buf)) 'up-to-date)))
                                  buf nil))
                            (buffer-list)))
      (setq buflist (delq nil buflist)))

    ;; only ask for commit on the last buffer under rootdir with changes
    (if (and rootdir
             (eq (length buflist) 0))
        (if (y-or-n-face-p (concat "Commit changes to repo at " rootdir "? ") 'abdo-commit-question)
            (progn (message "Preparing to commit")
                   (abdo-vcs-status rootdir) t)
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
      (setq rootdir (abdo-vcs-root (buffer-file-name buf)))
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


;; The following is based on this
;; https://github.com/kriesoff/emacs-save-yourself/blob/master/save-yourself.el

(defun abdo-save-buffers-new-frame ()
  "Ask to save buffers on a new frame"
  (let ((buffer-pred (lambda (buffer)
                       (and (buffer-live-p buffer)
                            (buffer-modified-p buffer)
                            (not (buffer-base-buffer buffer))
                            (or
                             (buffer-file-name buffer)
                             (with-current-buffer buffer
                               (and buffer-offer-save
                                    (> (buffer-size) 0)))))))

        (process-pred (lambda (process)
                        (and (memq (process-status process)
                                   '(run stop open listen))
                             (process-query-on-exit-flag process))))

        (filter (lambda (pred lst)
                  (let (aux)
                    (dolist (elem lst)
                      (when (funcall pred elem)
                        (setq aux (cons elem aux))))
                    (reverse aux)))))

    (let ((modified (funcall filter buffer-pred (buffer-list)))
          (active (and (not abdo-save-buffers-new-frame-kill-processes)
                       (funcall filter process-pred (process-list)))))

      (when (or modified active)
        (save-excursion
          (when (and (not (eq window-system 'x))
                     (not x-initialized))
            (x-initialize-window-system))
          (select-frame (make-frame-on-display (getenv "DISPLAY")
                                               (cons
                                                '(window-system . x)
                                                '((height . 30)
                                                  (top . 10)
                                                  (left . 10)))))

          (let ((buflist (when modified (list-buffers-noselect nil modified)))
                (proclsit (when active (get-buffer-create "*Process List*"))))

            (cond
             ((and modified active)
              (switch-to-buffer buflist)
              (switch-to-buffer-other-window proclist)
              (list-processes t))

             (modified
              (switch-to-buffer buflist))

             (active
              (switch-to-buffer proclist)
              (list-processes t))))

          (let* ((new-frame (selected-frame))
                 (inhibit-quit t)
                 (use-dialog-box nil)
                 (cancel (or (with-local-quit
                               (save-some-buffers)
                               (and active
                                    (not (yes-or-no-p
                                          "Active processes exist; exit anyway? "))))
                             quit-flag)))
            (setq quit-flag nil)
            (delete-frame new-frame)
            cancel))))))





(defun abdo-exit ()
  (interactive)
  (if abdo-commit-on-kill
      (abdo-save-buffers-kill-terminal-commit)
    (save-buffers-kill-terminal)))


(defun abdo-done ()
  (interactive)
  (if abdo-commit-on-kill
      (abdo-buffer-done-commit)
    (abdo-buffer-done)))



(provide 'abdo-session)
