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
      "r ." 'rtags-find-symbol-at-point
      "r ," 'rtags-find-references-at-point
      "r v" 'rtags-find-virtuals-at-point
      "r V" 'rtags-print-enum-value-at-point
      "r /" 'rtags-find-all-references-at-point
      "r Y" 'rtags-cycle-overlays-on-screen
      "r >" 'rtags-find-symbol
      "r <" 'rtags-find-references
      "r [" 'rtags-location-stack-back
      "r ]" 'rtags-location-stack-forward
      "r D" 'rtags-diagnostics
      "r G" 'rtags-guess-function-at-point
      "r p" 'rtags-set-current-project
      "r P" 'rtags-print-dependencies
      "r e" 'rtags-reparse-file
      "r E" 'rtags-preprocess-file
      "r R" 'rtags-rename-symbol
      "r M" 'rtags-symbol-info
      "r S" 'rtags-display-summary
      "r O" 'rtags-goto-offset
      "r ;" 'rtags-find-file
      "r F" 'rtags-fixit
      "r L" 'rtags-copy-and-print-current-location
      "r X" 'rtags-fix-fixit-at-point
      "r B" 'rtags-show-rtags-buffer
      "r I" 'rtags-imenu
      "r T" 'rtags-taglist
      "r h" 'rtags-print-class-hierarchy
      "r a" 'rtags-print-source-arguments))

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

