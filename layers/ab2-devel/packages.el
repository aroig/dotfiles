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
        ; ycmd
        (irony :location local)
        (company-irony :requires company)
        (flycheck-irony :requires flycheck)
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


(defun ab2-devel/init-compile ()
  (use-package compile :defer t)
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

(defun ab2/cc-mode-config ()
  ;; disable electric-indent. I'll use clang-format
  ;; (electric-indent-local-mode -1)
  (c-toggle-electric-state -1)

  ;; setup tab-always-indent locally
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)

  ;; Although I use clang format, this is useful while editing
  (setq c-syntactic-indentation t)

  ;; Base C++ style
  ;; (c-set-style "stroustrup")
  (c-add-style "google" ab2/google-c-style t)
  (c-set-offset 'access-label -2))

(defun ab2-devel/pre-init-cc-mode ()
  ;; flyspell for comments
  ; (flyspell-prog-mode)                    ;; Enable flyspell on C/C++ comments
  ; (abdo-change-dictionary "english")      ;; I always program in english

  ;; extra QT Keywords
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                 "\\|protected slot\\|private\\|private slot"
                                 "\\)\\>")
        c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                 "\\|public slots\\|protected slots\\|private slots"
                                 "\\)\\>[ \t]*:"))

  (add-hook 'c-mode-common-hook 'ab2/cc-mode-config))


(defun ab2-devel/post-init-cmake-mode ()
  (setq cmake-tab-width 4))

(defun ab2-devel/pre-init-ycmd ()
  (setq ycmd-server-command `(,(file-truename (concat dotspacemacs-directory "bin/ycmd")))
        ycmd-force-semantic-completion t
        ycmd-extra-conf-whitelist (file-truename (concat user-emacs-directory "layers"))
        )
  (spacemacs|diminish ycmd-mode "Ⓒ" " C"))

(defun ab2-devel/init-irony ()
  (use-package irony-mode
    :defer t
    :init
    (progn
      (setq irony-server-install-prefix "/usr")
      (setq irony--server-executable (concat dotspacemacs-directory "bin/irony-server"))
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      (spacemacs|diminish irony-mode "Ⓘ" " I"))))


(defun ab2-devel/init-company-irony ()
  (use-package company-irony
    :defer t
    :init
    (progn
      (push 'company-irony company-backends-c-mode-common)
      (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))))


(defun ab2-devel/init-flycheck-irony ()
  (use-package flycheck-irony
    :defer t
    :init
    (progn (add-hook 'irony-mode-hook 'flycheck-irony-setup))))
