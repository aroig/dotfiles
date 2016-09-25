;;; packages.el --- rtags layer for spacemacs

(setq ab2-clang-packages
      '(cc-mode
        clang-format
        projectile
        ))

(defun ab2-clang/init-clang-format ()
  (use-package clang-format))

(defun ab2-clang/post-init-cc-mode ()
  (add-hook 'c-mode-common-hook
            #'(lambda ()
                (projectile-mode)
                (when (and ab2-clang/enable-rtags (projectile-project-p))
                  (ab2-clang/load-rtags)
                  (ab2-clang/rtags-add-project (projectile-project-root)))
                )))

