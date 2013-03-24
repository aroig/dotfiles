
;; Vi mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/Evil

(defun evil-undefine ()
 (interactive)
 (let (evil-mode-map-alist)
   (call-interactively (key-binding (this-command-keys)))))


(defun abdo-vi-things ()
  ;; by default, start with emacs state.
  (setq evil-idefault-state 'emacs)

  ;; initial state by mode
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'text-mode 'normal)
  (evil-set-initial-state 'org-mode  'normal)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'inferior-python-mode 'emacs)

  (evil-set-initial-state 'rcirc-mode 'emacs)
  (evil-set-initial-state 'jabber-roster-mode 'emacs)
  (evil-set-initial-state 'jabber-chat-mode 'emacs)

  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'mu4e-main-mode 'emacs)
  (evil-set-initial-state 'mu4e-headers-mode 'emacs)
  (evil-set-initial-state 'mu4e-view-mode 'emacs)


  ;; org mode bindings
  (add-hook 'org-mode-hook (lambda ()
                             (define-key evil-normal-state-map (kbd "K") 'org-open-at-point)))


  ; preserve emacs keybindings.
;  (setcdr evil-insert-state-map nil)
;  (define-key evil-insert-state-map
;  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

  ;; evil keybindings
  ; (define-key evil-normal-state-map "\C-r" 'isearch-backward)


  ;; Misc config
  (setq evil-regexp-search t)                ; use regexps for searching

  ;; evil keybindings

  ;; Preserve some emacs keybindings
  (define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)
  (define-key evil-normal-state-map (kbd "C-y") 'yank)
  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-visual-state-map (kbd "C-y") 'yank)

  (define-key evil-normal-state-map (kbd "C-w") 'kill-region)
  (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
  (define-key evil-visual-state-map (kbd "C-w") 'kill-region)



  ;; Bind esc to H-a
  (define-key evil-normal-state-map (kbd "H-a") 'evil-force-normal-state)
  (define-key evil-visual-state-map (kbd "H-a") 'evil-exit-visual-state)
  (define-key evil-insert-state-map (kbd "H-a") 'evil-normal-state)
  (define-key evil-replace-state-map (kbd "H-a") 'evil-normal-state)
)

(evil-mode 1)
(abdo-vi-things)

(provide 'abdo-vi)
