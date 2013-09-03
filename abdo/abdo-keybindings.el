
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adjusting key mappings

; For some reason this does not work! I'd want to separate C-i from TAB
; (add-hook 'term-setup-hook
;          (lambda ()
  ;; Mappings
  ; don't map C-i to TAB!
  ; http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
  ; http://www.gnu.org/savannah-checkouts/gnu/emacs/manual/html_node/elisp/Function-Keys.html
;            (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
;            (global-set-key (kbd "C-i") 'forward-word)))


;; Helm
(global-set-key (kbd "H-y f") 'helm-mini)
(global-set-key (kbd "H-y t") 'abdo-helm-todos)
(global-set-key (kbd "H-y o") 'abdo-helm-org-files)
(global-set-key (kbd "H-y h") 'helm-org-headlines)


;; Org mode
(global-set-key (kbd "H-o l") 'org-store-link)
(global-set-key (kbd "H-o c") 'org-capture)
(global-set-key (kbd "H-o a") 'org-agenda)
(global-set-key (kbd "H-o b") 'org-iswitchb)

(global-set-key (kbd "H-o w") 'abdo-org-main-buffer)
(global-set-key (kbd "H-o h") 'abdo-org-home-view)
(global-set-key (kbd "H-o u") 'abdo-org-update-agenda)

(global-set-key (kbd "C-c C-x q") 'abdo-org-archive-done-tasks)


;; That should change quit-char to <. However, it is broken on emacs 24-1.
;; As C-g is hard-coded on many places.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=1218
;
; (set-quit-char "<")
; (global-set-key (kbd "C-<") 'keyboard-quit)

;; Emacs stuff
(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "H-c r") 'reload-emacs-config)
(global-set-key (kbd "H-c a") 'menu-bar-mode)
(global-set-key (kbd "H-c d") 'toggle-debug-on-error)
(global-set-key (kbd "H-c s") 'sr-speedbar-toggle)

;; ido buffer
(global-set-key (kbd "C-x C-b") 'abdo-ibuffer)

;; auto-complete
(global-set-key (kbd "<C-tab>") 'auto-complete)

;; Undo-Tree
(global-set-key (kbd "H-C-<prior>")  'undo-tree-undo)
(global-set-key (kbd "H-C-<next>")   'undo-tree-redo)

(global-set-key (kbd "<mouse-8>") 'undo-tree-undo)
(global-set-key (kbd "<mouse-9>") 'undo-tree-redo)


;; Disable close ones.
;; (global-set-key (kbd "C-<prior>") nil)
;; (global-set-key (kbd "C-<next>") nil)


;; Usability UNbindings
(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "<insert>"))

;; Ediff
(global-set-key (kbd "H-d d") 'abdo-ediff-current-file)
(global-set-key (kbd "H-d v") (lambda () (interactive) (abdo-ediff-current-file 4)))
(global-set-key (kbd "H-d t") 'abdo-fixme-search)

;; Launchers
(global-set-key (kbd "H-x t") 'abdo-launch-terminal)
(global-set-key (kbd "H-x f") 'abdo-launch-filemanager)
(global-set-key (kbd "H-c m") (lambda () (abdo-mu4 "mail")))
(global-set-key (kbd "H-c i") 'rcirc)
(global-set-key (kbd "H-h s") 'calibre-find)

;; Version Control
(global-set-key (kbd "H-v s") 'abdo-vcs-status)

;; Winner mode
(global-set-key (kbd "H-s-<prior>") 'winner-undo)
(global-set-key (kbd "H-s-<next>") 'winner-redo)

;; Windmove key bindings
(global-set-key (kbd "H-s-<left>")  'windmove-left)
(global-set-key (kbd "H-s-<right>") 'windmove-right)
(global-set-key (kbd "H-s-<up>")    'windmove-up)
(global-set-key (kbd "H-s-<down>")  'windmove-down)


;; Exit functions

(defvar abdo-commit-on-kill t "If non-nil asks to commit on kill")

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

;; Exit keybindings
(global-set-key (kbd "C-x C-z") 'abdo-exit)
(global-set-key (kbd "C-x C-<") 'abdo-done)
(if (daemonp)
    (progn
      (global-set-key (kbd "C-x C-c") 'abdo-done)
      (add-hook 'find-file-hook (lambda () (abdo-client-visit-buffer (current-buffer))))
    )
  (global-set-key (kbd "C-x C-c") 'abdo-exit))



;; Mode specific bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; server hook
; (add-hook 'server-visit-hook
;          (lambda ()
;            (local-set-key (kbd "C-x C-c") 'abdo-done)))


;; Outline mode
(defun outline-mode-keybindings ()
  (local-set-key (kbd "<C-tab>") 'outline-cycle) ; Cycle outline

  (local-set-key (kbd "H-<up>") 'outline-move-subtree-up)
  (local-set-key (kbd "H-<down>") 'outline-move-subtree-down)
  (local-set-key (kbd "H-<left>") 'outline-promote)
  (local-set-key (kbd "H-<right>") 'outline-demote))

(add-hook 'outline-mode-hook 'outline-mode-keybindings)
(add-hook 'outline-minor-mode-hook 'outline-mode-keybindings)


;; ediff
(add-hook 'ediff-load-hook
        (lambda ()
          ;; Don't want to be asked for commit when merging, etc.
          (make-local-variable 'abdo-commit-on-kill)
          (setq abdo-commit-on-kill nil)))


;; text mode
(add-hook 'text-mode-hook
      (lambda ()
        (abdo-flyspell-things)
        (local-set-key (kbd "s-c d") 'abdo-change-dictionary)))


;; Git commit
(add-hook 'git-commit-mode-hook
	  (lambda ()
        ;; enable flyspell
        (flyspell-mode 1)

	    ;; Don't want to be asked for commit when commiting!
        (make-local-variable 'abdo-commit-on-kill)
        (setq abdo-commit-on-kill nil)))


;; Git rebase
(add-hook 'rebase-mode-hook
	  (lambda ()
	    ;; Don't want to be asked for commit when rebasing!
            (make-local-variable 'abdo-commit-on-kill)
            (setq abdo-commit-on-kill nil)))


;; Latex
(add-hook 'LaTeX-mode-hook
	  (lambda ()
        ;; enable flyspell
        (flyspell-mode 1)

	    (local-set-key (kbd "C-c c") 'abdo-latex-compile-buffer)   ;; Compile
	    (local-set-key (kbd "C-c k") 'kill-compilation)            ;; Kill compilation
	    (local-set-key (kbd "C-c d") 'abdo-latex-make-diff)        ;; Makes a diff

	    (local-set-key (kbd "C-c e") 'abdo-latex-show-compilation) ;; Show compilation
	    (local-set-key (kbd "C-c n") 'next-error)                  ;; next error
	    (local-set-key (kbd "C-c p") 'previous-error)              ;; previous error

	    (local-set-key (kbd "C-c r") 'abdo-latex-toggle-toc)       ;; Toggles the reftex toc
	    (local-set-key (kbd "C-c v") 'abdo-latex-view)))           ;; View


;; C/C++
(defun abdo-c-mode-keybindings()
            (local-set-key (kbd "C-c c") 'compile)                    ;; Compile
            (local-set-key (kbd "C-c g") 'gdb)                        ;; gdb

            (local-set-key (kbd "C-c n") 'next-error)                 ;; next error
            (local-set-key (kbd "C-c p") 'previous-error))            ;; previous error

(add-hook 'c++-mode-hook 'abdo-c-mode-keybindings)
(add-hook 'c-mode-hook 'abdo-c-mode-keybindings)


;; org mode
(add-hook 'org-mode-hook
        (lambda ()
          (local-set-key (kbd "s-c y") 'doku-import-yank)
          (local-set-key (kbd "H-o p") 'abdo-org-latex-preview-all)))

;; rcirc mode
(add-hook 'rcirc-mode-hook
        (lambda ()
          (local-set-key (kbd "C-c C-x")
            (lambda () (interactive) (rcirc-cmd-quit "bye")))))

;; mail
(add-hook 'mu4e-compose-mode-hook
        (lambda ()
          ;; enable flyspell
          (flyspell-mode 1)))



(provide 'abdo-keybindings)
