(setq ab2-devel-packages
      '(
        magit
        pkgbuild-mode
        haskell-mode
        lua-mode
        python-mode
        compile
        cc-mode
        helm-make
        cmake-mode
        projectile
        lsp-mode
        ))


(defun ab2-devel/pre-init-magit ()
  ;; Since I use git outside of emacs, do not rely on magit-auto-revert-mode.
  (setq magit-auto-revert-mode nil)

  )

(defun ab2-devel/post-init-pkgbuild-mode ()
  (setq pkgbuild-initialize nil
        pkgbuild-update-sums-on-save nil))


(defun ab2-devel/pre-init-helm-make ()
  ;; run make -qp to extract list of targets from Makefile
  (setq helm-make-list-target-method 'qp
        helm-make-executable "make --no-print-directory"))


(defun ab2-devel/post-init-compile ()
  (setq compilation-read-command nil
        compilation-auto-jump-to-first-error nil
        compilation-scroll-output 'first-error
        compilation-finish-function nil
        compilation-environment '("TERM=ansi"))

  ;; When compilation  finishes
  (add-to-list 'compilation-finish-functions #'ab2/compilation-finished))


(defun ab2-devel/post-init-haskell-mode ()

  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  )


(defun ab2-devel/post-init-lua-mode ()
  (add-hook 'lua-mode-hook
            (lambda ()
              (setq lua-indent-level 4)
              (editorconfig-apply)
              ))
  )


(defun ab2-devel/post-init-python-mode ()
  (add-hook 'python-mode-hook
            (lambda ()
              (setq python-shell-interpreter "ipython"
                    python-indent-offset 4)

              ;;  (flyspell-prog-mode)                    ;; Enable flyspell on C/C++ comments
              ;;  (abdo-change-dictionary "english")      ;; I always program in english

              (editorconfig-apply)
              ))
  )

(defun ab2-devel/pre-init-cc-mode ()
  ;; flyspell for comments
  ; (flyspell-prog-mode)                    ;; Enable flyspell on C/C++ comments
  ; (abdo-change-dictionary "english")      ;; I always program in english

  ;; do not auto-indent on paste
  (add-to-list 'spacemacs-indent-sensitive-modes 'c-mode)
  (add-to-list 'spacemacs-indent-sensitive-modes 'c++-mode)

  ;; settings
  (setq
    c-tab-always-indent t
    c-syntactic-indentation nil
    c-electric-flag nil)

  ;; extra QT Keywords
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                 "\\|protected slot\\|private\\|private slot"
                                 "\\)\\>")
        c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                 "\\|public slots\\|protected slots\\|private slots"
                                 "\\)\\>[ \t]*:"))

  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; Base C++ style
              ;; (c-set-style "stroustrup")
              (c-add-style "google" ab2/google-c-style t)
              (c-set-offset 'access-label -2)

              ;; Disable electric indent
              (setq c-electric-flag nil)

              ;; Apply editorconfig settings
              (editorconfig-apply))))

(defun ab2-devel/post-init-lsp-mode ()
  (setq lsp-restart 'ignore)
  (spacemacs|diminish lsp-mode "Ⓛ" " L"))


(defun ab2-devel/post-init-cmake-mode ()
  (add-hook 'cmake-mode-hook
            (lambda ()
              (setq cmake-tab-width 4)
              (editorconfig-apply))))

(defun ab2-devel/post-init-projectile()
  (setq projectile-track-known-projects-automatically nil))
