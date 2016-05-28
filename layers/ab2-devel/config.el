
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
        editorconfig
        helm-make
        ansi-color
        ))

;; compile buffer
(defvar ab2/compile-window-state nil)

;; Basic prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil        ;; No tabs on indent
                          tab-width 4)
            ))

;; enable auto-fill on programming modes
;; (add-hook 'prog-mode-hook 'turn-on-auto-fill)

