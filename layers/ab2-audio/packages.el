(setq ab2-audio-packages
      '(extempore-mode)
      )

(defun ab2-audio/init-extempore-mode ()
  (use-package extempore-mode
    :mode ("\\.xtm\\'" . extempore-mode)
    :defer t))
