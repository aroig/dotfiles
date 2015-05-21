
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
  (let ((path (expand-file-name default-directory))
        (cmd (getenv "TERMCMD")))
    (call-process cmd nil 0 nil "-d" path)))



;; Launch file manager on current directory
(defun abdo-launch-filemanager ()
  (interactive)
  (let ((path (expand-file-name default-directory))
        (cmd (getenv "FILEMANAGER")))
    (call-process cmd nil 0 nil path)))



;; recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/RecentFiles

;; There is a tiny window of opportunity for two emacs processes to write to
;; recentf at the same time. If this happens with two emacsen running as a
;; server when they shut down, one of them becomes deadlocked.
;; Let's only use recentf for the main emacs daemon.
(when (and (server-running-p) (string= server-name "server"))
  (setq recentf-save-file (convert-standard-filename "~/.recentf"))
  (setq recentf-auto-cleanup 'never)  ;; need it because of tramp
  (recentf-mode 1)
  (setq recentf-max-saved-items 500))



;; Auto Revert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: There is a problem here. revert-buffer in file.el.gz calls
;; insert-file-contents with a fifth argument preventing a full "remove all"
;; "insert all" change in the undo list. This leads to a broken undo-list.
;;
;; NOTE: It seems that emacs 24.4 will fix this issue!

(setq auto-revert-mode-text " ar")     ;; auto-revert modeline string
(setq auto-revert-check-vc-info t)     ;; update vcs state

;; Enable auto-revert prog and text buffers
(add-hook 'prog-mode-hook (lambda () (auto-revert-mode)))
(add-hook 'text-mode-hook (lambda () (auto-revert-mode)))



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



;; Speedbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sr-speedbar-right-side nil)
(setq speedbar-use-images nil)


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

;; Let's keep a stack of window configurations
(defvar winstack-stack '()
  "A Stack holding window configurations.
Use `winstack-push' and
`winstack-pop' to modify it.")

(defun winstack-push()
  "Push the current window configuration onto `winstack-stack'."
  (interactive)
  (if (and (window-configuration-p (first winstack-stack))
           (compare-window-configurations (first winstack-stack) (current-window-configuration)))
      (message "Current config already pushed")
    (progn (push (current-window-configuration) winstack-stack)
           (message (concat "pushed " (number-to-string
                                       (length (window-list (selected-frame)))) " frame config")))))

(defun winstack-pop()
  "Pop the last window configuration off `winstack-stack' and apply it."
  (interactive)
  (if (first winstack-stack)
      (progn (set-window-configuration (pop winstack-stack))
             (message "popped"))
    (message "End of window stack")))


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

;; Let's prevent ido from saving state data to disk
; (setq ido-save-directory-list-file (convert-standard-filename "~/.ido.last"))
(setq ido-save-directory-list-file nil)

;; enable ido with flx-mode
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)

;; disable ido faces to see flx highlights.
;; (setq ido-use-faces nil)

;; tuning
(setq ido-enable-flex-matching t)
(setq ido-max-prospects 8)
(setq ido-rotate t)
(setq ido-enable-regexp t)

;; (setq ido-use-filename-at-point t)
(setq ido-auto-merge-work-directories-length -1)

;; always open buffer in selected-window
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)



;; Ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/EdiffMode

;; Clean up buffers after diff is done
(add-hook 'ediff-cleanup-hook (lambda () (ediff-janitor nil nil)))

;; Single frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Always split horizontally
(setq ediff-split-window-function 'split-window-horizontally)

;; Split either vertically or horizontally. Depending on frame size
; (setq ediff-split-window-function (if (> (frame-width) 150)
;                                           'split-window-horizontally
;                                        'split-window-vertically))

;; Hooks to save and restore window configuration
(add-hook 'ediff-before-setup-hook 'winstack-push)
(add-hook 'ediff-quit-hook 'winstack-pop 'append)
(add-hook 'ediff-suspend-hook 'winstack-pop 'append)





;; Emacs 1on1. Disabled because seems quite unmantained
;; (setq 1on1-minibuffer-frame-flag nil)
;; (require 'oneonone)
;; (1on1-emacs)



(provide 'abdo-interactive)
