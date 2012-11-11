
;; Vi mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/Evil


(defun evil-undefine ()
 (interactive)
 (let (evil-mode-map-alist)
   (call-interactively (key-binding (this-command-keys)))))

(defun abdo-vi-things ()
  ;; by default, start with emacs state.
  (setq evil-default-state 'emacs)

  ;; Actions specific to certain modes. Mainly setup default state.
  (add-hook 'text-mode-hook (lambda ()
			      (make-local-variable 'evil-default-state)
			      (setq evil-default-state 'normal)))

  (add-hook 'prog-mode-hook (lambda ()
			      (make-local-variable 'evil-default-state)
			      (setq evil-default-state 'normal)))

  (add-hook 'inferior-python-mode-hook (lambda ()
			      (make-local-variable 'evil-default-state)
			      (setq evil-default-state 'emacs)))

  (add-hook 'lua-mode-hook (lambda ()
			      (make-local-variable 'evil-default-state)
			      (setq evil-default-state 'normal)))

  (add-hook 'git-commit-mode-hook (lambda ()
			      (make-local-variable 'evil-default-state)
			      (setq evil-default-state 'insert)))

  (add-hook 'org-mode-hook (lambda ()
 			     (define-key evil-normal-state-map (kbd "K") 'org-open-at-point)
			     (make-local-variable 'evil-default-state)
			     (setq evil-default-state 'normal)))

  ;; I can setup initial state with evil-set-initial-state. But then I would
  ;; have to go mode by mode.  If I use hooks, I can use the fact that many
  ;; buffers derive from text-mode and run text-mode hook!



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
