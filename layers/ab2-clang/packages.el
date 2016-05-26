;;; packages.el --- rtags layer for spacemacs

(setq ab2-clang-packages
      '(rtags
        company
        flycheck
        clang-format
        projectile
        ))

(defun ab2-clang/init-clang-format ()
  (use-package clang-format))

(defun ab2-clang/post-init-company ()
  (push 'company-rtags company-backends-c-mode-common))

(defun ab2-clang/post-init-flycheck ()
  (add-hook 'c-mode-common-hook
            #'(lambda ()
                (flycheck-select-checker 'rtags)
                (setq-local flycheck-check-syntax-automatically nil)
                (setq-local flycheck-highlighting-mode nil)
                )))

(defun ab2-clang/init-rtags ()
  ;; cannot use deferred with locally installed version. has no autoloads.
  (use-package rtags
    :config
    (when (configuration-layer/package-usedp 'company) (use-package company-rtags))
    (when (configuration-layer/package-usedp 'flycheck) (use-package flycheck-rtags))))

(defun ab2-clang/post-init-projectile ()
    (add-hook 'projectile-mode-hook
              #'(lambda ()
                  (when (projectile-project-p)
                    (ab2-clang/rtags-add-project (projectile-project-root))))))

(defun ab2-clang/post-init-rtags ()
  (setq rtags-completions-enabled t
        company-rtags-begin-after-member-access nil
        rtags-autostart-diagnostics t)

  (defun use-rtags (&optional useFileManager)
    (and (rtags-executable-find "rc")
         (cond ((not (gtags-get-rootpath)) t)
               ((and (not (eq major-mode 'c++-mode))
                     (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
               (useFileManager (rtags-has-filemanager))
               (t (rtags-is-indexed)))))

  (defun tags-find-symbol-at-point (&optional prefix)
    (interactive "P")
    (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
        (helm-gtags-find-tag)))

  (defun tags-find-references-at-point (&optional prefix)
    (interactive "P")
    (if (and (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
        (helm-gtags-find-rtag)))

  (defun tags-find-symbol ()
    (interactive)
    (call-interactively (if (use-rtags) 'rtags-find-symbol 'helm-gtags-find-symbol)))

  (defun tags-find-references ()
    (interactive)
    (call-interactively (if (use-rtags) 'rtags-find-references 'helm-gtags-find-rtag)))

  (defun tags-find-file ()
    (interactive)
    (call-interactively (if (use-rtags t) 'rtags-find-file 'helm-gtags-find-files)))

  (defun tags-imenu ()
    (interactive)
    (call-interactively (if (use-rtags t) 'rtags-imenu 'idomenu)))

  (dolist (mode '(c-mode c++-mode))
    (evil-leader/set-key-for-mode mode
      "t ." 'rtags-find-symbol-at-point
      "t ," 'rtags-find-references-at-point
      "t v" 'rtags-find-virtuals-at-point
      "t V" 'rtags-print-enum-value-at-point
      "t /" 'rtags-find-all-references-at-point
      "t Y" 'rtags-cycle-overlays-on-screen
      "t >" 'rtags-find-symbol
      "t <" 'rtags-find-references
      "t [" 'rtags-location-stack-back
      "t ]" 'rtags-location-stack-forward
      "t D" 'rtags-diagnostics
      "t G" 'rtags-guess-function-at-point
      "t p" 'rtags-set-current-project
      "t P" 'rtags-print-dependencies
      "t e" 'rtags-reparse-file
      "t E" 'rtags-preprocess-file
      "t R" 'rtags-rename-symbol
      "t M" 'rtags-symbol-info
      "t S" 'rtags-display-summary
      "t O" 'rtags-goto-offset
      "t ;" 'rtags-find-file
      "t F" 'rtags-fixit
      "t L" 'rtags-copy-and-print-current-location
      "t X" 'rtags-fix-fixit-at-point
      "t B" 'rtags-show-rtags-buffer
      "t I" 'rtags-imenu
      "t T" 'rtags-taglist
      "t h" 'rtags-print-class-hierarchy
      "t a" 'rtags-print-source-arguments))

  (rtags-enable-standard-keybindings)
  (define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,") (function tags-find-references-at-point))
  (define-key c-mode-base-map (kbd "M-;") (function tags-find-file))
  (define-key c-mode-base-map (kbd "C-.") (function tags-find-symbol))
  (define-key c-mode-base-map (kbd "C-,") (function tags-find-references))
  (define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
  (define-key c-mode-base-map (kbd "M-i") (function tags-imenu))

  (define-key global-map (kbd "M-.") (function tags-find-symbol-at-point))
  (define-key global-map (kbd "M-,") (function tags-find-references-at-point))
  (define-key global-map (kbd "M-;") (function tags-find-file))
  (define-key global-map (kbd "C-.") (function tags-find-symbol))
  (define-key global-map (kbd "C-,") (function tags-find-references))
  (define-key global-map (kbd "C-<") (function rtags-find-virtuals-at-point))
  (define-key global-map (kbd "M-i") (function tags-imenu)))

