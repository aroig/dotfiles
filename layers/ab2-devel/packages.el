(setq ab2-devel-packages
      '(
        magit
        persp-mode
        pkgbuild-mode
        compilation
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

(defun ab2-devel/post-init-compilation ()
  (setq compilation-read-command nil
        compilation-auto-jump-to-first-error t)

  )
