(setq ab2-base-packages
      '(
        (sensitive-mode :location local)
        ))

(defun ab2-base/init-sensitive-mode ()

  (use-package sensitive-mode)

  ;; disable backups for irrelevant emacs files
  (add-to-list 'auto-mode-alist '("\\.ido\\.last" . sensitive-mode))
  (add-to-list 'auto-mode-alist '("\\.recentf" . sensitive-mode))
  (add-to-list 'auto-mode-alist '("spacemacs-buffer.el" . sensitive-mode))

  ;; disable backups for sensitive files
  (add-to-list 'auto-mode-alist '("\\.gpg$" . sensitive-mode))
  (add-to-list 'auto-mode-alist `(,(ab2/escape-regexp (getenv "AB2_PRIV_DIR")) . sensitive-mode))
  (add-to-list 'auto-mode-alist `(,(ab2/escape-regexp (file-truename "~/.ssh")) . sensitive-mode))
  )
