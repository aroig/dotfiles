(setq ab2-base-packages
      '(
        company
        uniquify
        hippie-exp
        (sensitive-mode :location local)
        ))


(defun ab2-base/post-init-uniquify ()
  (setq
   ;; uniquify buffer names as <dir>/<name>
   uniquify-buffer-name-style 'forward
   ))


(defun ab2-base/init-sensitive-mode ()
  (use-package sensitive-mode)

  ;; disable backups for irrelevant emacs files
  (add-to-list 'auto-mode-alist '("recentf$" . sensitive-mode))
  (add-to-list 'auto-mode-alist '("spacemacs-buffer\\.el$" . sensitive-mode))

  ;; disable backups for sensitive files
  (add-to-list 'auto-mode-alist '("\\.gpg$" . sensitive-mode))
  (add-to-list 'auto-mode-alist `(,(ab2/escape-regexp (getenv "AB2_PRIV_DIR")) . sensitive-mode))
  (add-to-list 'auto-mode-alist `(,(ab2/escape-regexp (file-truename "~/.ssh")) . sensitive-mode))
  )

(ab2/regexp-filter "spacemacs-buffer\\.el" '("spacemacs-buffer.el"))

(defun ab2-base/post-init-hippie-exp ()
  ;; I prefer a diferent binding for yasnippet completion
  (global-set-key (kbd "M-+") 'hippie-expand)
  )

(defun ab2-base/post-init-autocomplete ()
  (setq
   tab-always-indent t
   company-idle-delay 0.4
  ))
