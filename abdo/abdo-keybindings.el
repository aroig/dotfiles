
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adjusting key mappings

;; For some reason this does not work! I'd want to separate C-i from TAB

;; http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
;; http://www.gnu.org/savannah-checkouts/gnu/emacs/manual/html_node/elisp/Function-Keys.html

;; NOTE: this doesn't seem to work (2015-05-08)
; (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))

;; NOTE: I can bind to <tab> and do not touch C-i though!
; (global-set-key (kbd "<tab>") (lambda () (interactive) (message "<tab>")))


;; grep
(global-set-key (kbd "C-c g") 'rgrep)

;; Helm
(global-set-key (kbd "H-y f") 'helm-mini)
(global-set-key (kbd "H-y t") 'abdo-helm-todos)
(global-set-key (kbd "H-y o") 'abdo-helm-org-files)
(global-set-key (kbd "H-y h") 'helm-org-headlines)
(global-set-key (kbd "H-y p") 'abdo-helm-papers)
(global-set-key (kbd "H-y g") 'helm-do-grep)
(global-set-key (kbd "H-y u") 'helm-mu)


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
(global-set-key (kbd "H-c m") 'abdo-mu4)
(global-set-key (kbd "H-c i") 'rcirc)
(global-set-key (kbd "H-c t") 'ansi-term)
(global-set-key (kbd "H-h s") 'calibre-find)

;; Version Control
(global-set-key (kbd "H-v s") 'abdo-vcs-status)
(global-set-key (kbd "H-v b") 'abdo-vcs-branches)
(global-set-key (kbd "H-v l") 'abdo-vcs-log)

;; Winner mode
(global-set-key (kbd "H-s-<prior>") 'winner-undo)
(global-set-key (kbd "H-s-<next>") 'winner-redo)

(global-set-key (kbd "H-s-o") 'winner-undo)
(global-set-key (kbd "H-s-i") 'winner-redo)

;; Windmove key bindings
(global-set-key (kbd "H-s-<left>")  'windmove-left)
(global-set-key (kbd "H-s-<right>") 'windmove-right)
(global-set-key (kbd "H-s-<up>")    'windmove-up)
(global-set-key (kbd "H-s-<down>")  'windmove-down)

(global-set-key (kbd "H-s-h")  'windmove-left)
(global-set-key (kbd "H-s-l") 'windmove-right)
(global-set-key (kbd "H-s-k")    'windmove-up)
(global-set-key (kbd "H-s-j")  'windmove-down)

;; Yasnippet
(define-key yas-minor-mode-map (kbd "<H-tab>")  'yas-expand)
(define-key yas-minor-mode-map (kbd "<tab>")     nil)
(define-key yas-minor-mode-map (kbd "TAB")       nil)

;; Autocomplete
(ac-set-trigger-key "C-TAB")

;; Exit keybindings
(global-set-key (kbd "C-x C-z") 'abdo-exit)
(global-set-key (kbd "C-x C-<") 'abdo-done)
(if (daemonp)
    (progn
      (global-set-key (kbd "C-x C-c") 'abdo-done)
      (add-hook 'find-file-hook (lambda () (abdo-client-visit-buffer (current-buffer)))))
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

  (local-set-key (kbd "H-l <up>") 'outline-move-subtree-up)
  (local-set-key (kbd "H-l <down>") 'outline-move-subtree-down)
  (local-set-key (kbd "H-l <left>") 'outline-promote)
  (local-set-key (kbd "H-l <right>") 'outline-demote)

  (local-set-key (kbd "H-l K") 'outline-move-subtree-up)
  (local-set-key (kbd "H-l J") 'outline-move-subtree-down)
  (local-set-key (kbd "H-l H") 'outline-promote)
  (local-set-key (kbd "H-l L") 'outline-demote)

  (local-set-key (kbd "H-l n") 'outline-next-visible-heading)
  (local-set-key (kbd "H-l p") 'outline-previous-visible-heading)
  (local-set-key (kbd "H-l f") 'outline-forward-same-level)
  (local-set-key (kbd "H-l b") 'outline-backward-same-level)
  (local-set-key (kbd "H-l u") 'outline-up-heading)
  )

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


;; Magit
(add-hook 'magit-mode-hook
          (lambda ()
            (local-set-key (kbd "H-v r") 'magit-interactive-resolve-item)))


;; Latex
(add-hook 'LaTeX-mode-hook
	  (lambda ()
        ;; enable flyspell
        (flyspell-mode 1)

	    (local-set-key (kbd "C-c c") 'abdo-latex-compile)          ;; Compile
	    (local-set-key (kbd "C-c k") 'kill-compilation)            ;; Kill compilation
	    (local-set-key (kbd "C-c d") 'abdo-latex-make-diff)        ;; Makes a diff

	    (local-set-key (kbd "C-c e") 'abdo-latex-show-compilation) ;; Show compilation
	    (local-set-key (kbd "C-c n") 'next-error)                  ;; next error
	    (local-set-key (kbd "C-c p") 'previous-error)              ;; previous error

	    (local-set-key (kbd "C-c r") 'abdo-latex-toggle-toc)       ;; Toggles the reftex toc
	    (local-set-key (kbd "C-c v") 'abdo-latex-view)             ;; View
        (local-set-key (kbd "C-c f") 'abdo-latex-forward-sync)     ;; Forward search

        (local-set-key (kbd "C-c j") 'abdo-latex-goto-label)       ;; Jump to a given label
        (local-set-key (kbd "C-c l") 'abdo-latex-insert-ref)       ;; Insert a \ref
        ))


;; C/C++
(defun abdo-c-mode-keybindings()
            (local-set-key (kbd "C-c c") 'abdo-devel-compile)         ;; Compile
            (local-set-key (kbd "C-c d") 'gdb)                        ;; gdb

            (local-set-key (kbd "C-c n") 'next-error)                 ;; next error
            (local-set-key (kbd "C-c p") 'previous-error))            ;; previous error

(add-hook 'c++-mode-hook 'abdo-c-mode-keybindings)
(add-hook 'c-mode-hook 'abdo-c-mode-keybindings)
(add-hook 'qml-mode-hook 'abdo-c-mode-keybindings)


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


;; yasnippets
(add-hook 'yas-minor-mode-hook
          (lambda ()
))


(provide 'abdo-keybindings)
