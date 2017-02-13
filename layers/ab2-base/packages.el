(setq ab2-base-packages
      '(
        evil
        company
        uniquify
        hippie-exp
        (sensitive-mode :location local)
        persp-mode
        ))


(defun ab2-base/post-init-evil ()
  ;; Attempt to fix undo problems in emacs 25.1
  ;; https://github.com/syl20bnr/spacemacs/issues/6203
  (setq evil-in-single-undo t))

(defun ab2-base/post-init-uniquify ()
  (setq
   ;; uniquify buffer names as <dir>/<name>
   uniquify-buffer-name-style 'forward
   ))


(defun ab2-base/init-sensitive-mode ()
  (use-package sensitive-mode)

  (spacemacs|diminish sensitive-mode "😎" "O-O¬")

  ;; disable backups for internal emacs files, or files containing sensitive
  ;; information
  (setq ab2/sensitive-mode-regex-list `("/recentf$"
                                        "/ido.last$"
                                        "/spacemacs-buffer\\.el$"
                                        "\\.gpg$"
                                        ,(ab2/escape-regexp (getenv "AB2_PRIV_DIR"))
                                        ,(ab2/escape-regexp (file-truename "~/.ssh"))))

  ;; enable sensitive-mode
  (add-hook 'find-file-hook (lambda ()
                              (when buffer-file-name
                                (let* ((name (file-name-sans-versions buffer-file-name))
                                       (remote-id (file-remote-p buffer-file-name)))
                                  (dolist (regex ab2/sensitive-mode-regex-list)
                                    (when (string-match regex name) (sensitive-mode))))))))

(defun ab2-base/post-init-hippie-exp ()
  ;; I prefer a diferent binding for yasnippet completion
  (global-set-key (kbd "M-+") 'hippie-expand)
  )

(defun ab2-base/post-init-company ()
  (setq
   tab-always-indent t
   company-idle-delay 0.4
  ))

(defun ab2-base/post-init-persp-mode ()
  (spacemacs|diminish persp-mode "" ""))
