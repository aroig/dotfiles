(setq ab2-science-packages
      '(
        (sage-mode :location (recipe :fetcher bitbucket :repo "gvol/sage-mode" :files ("emacs/*.el")))
        ))

(defun ab2-science/init-sage-mode ()
  (use-package sage-mode))

