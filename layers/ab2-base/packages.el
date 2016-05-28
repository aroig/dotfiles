
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

(defun ab2-base/post-init-hippie-exp ()
  ;; I prefer a diferent binding for yasnippet completion
  (global-set-key (kbd "M-+") 'hippie-expand)
  )

(defun ab2-base/post-init-autocomplete ()
  (setq
   tab-always-indent t
   company-idle-delay 0.4
  ))

(defun ab2-base/post-init-spaceline ()
  ;; override default version-control segment

  (spaceline-define-segment version-control
    "Personalized version control information"
    (when vc-mode
      (powerline-raw
       (s-trim (concat
                (replace-regexp-in-string "Git." "⎇ " vc-mode)
                (when (buffer-file-name)
                  (pcase (vc-state (buffer-file-name))
                    (`up-to-date " ✓")
                    (`edited " *")
                    (`added " +")
                    (`unregistered " ?")
                    (`removed " -")
                    (`needs-merge " X")
                    (`needs-update " *")
                    (`ignored " ·")
                    (_ " ?"))))))))

    (spaceline-define-segment buffer-position
      "The current approximate buffer position, in percent."
      (powerline-raw
       (replace-regexp-in-string
        "%" "%%"
        (downcase (substring (format-mode-line "%p") 0 3)))))

    ;; disable HUD showing which part of the buffer
    (spaceline-toggle-hud-off))
