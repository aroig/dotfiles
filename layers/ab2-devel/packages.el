(setq ab2-devel-packages
      '(
        magit
        persp-mode
        pkgbuild-mode
        haskell-mode
        lua-mode
        python-mode
        compile
        cc-mode
        clang-format
        ))


(defun ab2-devel/pre-init-magit ()
  ;; Since I use git outside of emacs, do not rely on magit-auto-revert-mode.
  (setq magit-auto-revert-mode nil)

  )

(defun ab2-devel/init-pkgbuild-mode ()
  (use-package pkgbuild-mode)

  (setq pkgbuild-initialize nil
        pkgbuild-update-sums-on-save nil)
  )

(defun ab2-devel/init-compile ()
  (use-package compile)
  (setq compilation-read-command nil
        compilation-auto-jump-to-first-error t
        compilation-scroll-output 'first-error
        compilation-finish-function nil)

  ;; When compilation  finishes
  (add-to-list 'compilation-finish-functions 'ab2/compilation-finished)
  )


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
              ))
  )


(defun ab2-devel/post-init-python-mode ()
  (add-hook 'python-mode-hook
            (lambda ()
              (setq python-shell-interpreter "ipython"
                    python-indent-offset 4)

              ;;  (flyspell-prog-mode)                    ;; Enable flyspell on C/C++ comments
              ;;  (abdo-change-dictionary "english")      ;; I always program in english
              ))
  )

(defun ab2-devel/post-init-cc-mode ()
  ;; flyspell for comments
  ; (flyspell-prog-mode)                    ;; Enable flyspell on C/C++ comments
  ; (abdo-change-dictionary "english")      ;; I always program in english

  ;; use rtags needs a daemon running!
  ;; (require 'rtags)

  ;; enable rtags bindings with C-cr prefix. I can't use H-r due to rtags limitations.
  ;; (rtags-enable-standard-keybindings)

  ;; Delete trailing whitespaces before save
  ;; (trailing-whitespace-mode)

  ;; extra QT Keywords
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                 "\\|protected slot\\|private\\|private slot"
                                 "\\)\\>")
        c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                 "\\|public slots\\|protected slots\\|private slots"
                                 "\\)\\>[ \t]*:"))

  )

(defun ab2-devel/post-init-clang-format ()
  ;; we use clang-format for indentation
  (setq c-syntactic-indentation nil)
  ;; (c-set-style "stroustrup")

  (add-hook 'c++-mode-hook
            (lambda ()
              ;; disable electric-indent. I'll use clang-format
              ;; (electric-indent-local-mode -1)
              (c-toggle-electric-state -1)
              ))

  (add-hook 'c-mode-hook
            (lambda ()
              ;; disable electric-indent. I'll use clang-format
              ;; (electric-indent-local-mode -1)
              (c-toggle-electric-state -1)
              ))
  )

