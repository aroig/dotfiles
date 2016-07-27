
;; registers a project with rtags if it finds a compilation database at the
;; root.
(defun ab2-clang/rtags-add-project (root)
  (let ((compdb (concat (directory-file-name root) "/compile_commands.json")))
    (when (file-exists-p compdb)
      (message (format "Starting rtags with compilation database %s" compdb))
      (with-temp-buffer (rtags-call-rc "-J" root)))))


;; Those come from the rtags README: https://github.com/Andersbakken/rtags

(defun ab2-clang/use-rtags (&optional useFileManager)
  (and (rtags-executable-find "rc")
       (cond ((not (gtags-get-rootpath)) t)
             ((and (not (eq major-mode 'c++-mode))
                   (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
             (useFileManager (rtags-has-filemanager))
             (t (rtags-is-indexed)))))

(defun ab2-clang/rtags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
      (helm-gtags-find-tag)))

(defun ab2-clang/rtags-find-references-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
      (helm-gtags-find-rtag)))

(defun ab2-clang/rtags-find-symbol ()
  (interactive)
  (call-interactively (if (ab2-clang/use-rtags) 'rtags-find-symbol 'helm-gtags-find-symbol)))

(defun ab2-clang/rtags-find-references ()
  (interactive)
  (call-interactively (if (ab2-clang/use-rtags) 'rtags-find-references 'helm-gtags-find-rtag)))

(defun ab2-clang/rtags-find-file ()
  (interactive)
  (call-interactively (if (ab2-clang/use-rtags t) 'rtags-find-file 'helm-gtags-find-files)))

(defun ab2-clang/rtags-imenu ()
  (interactive)
  (call-interactively (if (ab2-clang/use-rtags t) 'rtags-imenu 'idomenu)))

(defun ab2-clang/setup-rtags-bindings ()
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
  (define-key c-mode-base-map (kbd "M-.") (function ab2-clang/rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,") (function ab2-clang/rtags-find-references-at-point))
  (define-key c-mode-base-map (kbd "M-;") (function ab2-clang/rtags-find-file))
  (define-key c-mode-base-map (kbd "C-.") (function ab2-clang/rtags-find-symbol))
  (define-key c-mode-base-map (kbd "C-,") (function ab2-clang/rtags-find-references))
  (define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
  (define-key c-mode-base-map (kbd "M-i") (function ab2-clang/rtags-imenu))

  (define-key global-map (kbd "M-.") (function ab2-clang/rtags-find-symbol-at-point))
  (define-key global-map (kbd "M-,") (function ab2-clang/rtags-find-references-at-point))
  (define-key global-map (kbd "M-;") (function ab2-clang/rtags-find-file))
  (define-key global-map (kbd "C-.") (function ab2-clang/rtags-find-symbol))
  (define-key global-map (kbd "C-,") (function ab2-clang/rtags-find-references))
  (define-key global-map (kbd "C-<") (function rtags-find-virtuals-at-point))
  (define-key global-map (kbd "M-i") (function ab2-clang/rtags-imenu))
  )

;; Ideally, we should load rtags in the layer, and enable an rtags-mode on
;; C-mode hooks. Unfortunately, rtags does global things on loading, so we
;; actually need to load it from the C-mode hook itself.
(defun ab2-clang/load-rtags ()
  (setq rtags-completions-enabled t
        rtags-use-helm t
        company-rtags-begin-after-member-access nil
        rtags-autostart-diagnostics nil)

  (use-package rtags
    :config
    (when (configuration-layer/package-usedp 'company) (use-package company-rtags))
    (when (configuration-layer/package-usedp 'flycheck) (use-package flycheck-rtags))
    )

  ;; completion
  (push 'company-rtags company-backends-c-mode-common)

  ;; flycheck
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-check-syntax-automatically nil)
  (setq-local flycheck-highlighting-mode nil)
)
