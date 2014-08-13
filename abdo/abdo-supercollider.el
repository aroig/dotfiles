

(defun abdo-supercollider-things ()
  (setq sclang-auto-scroll-post-buffer t)
  (setq sclang-eval-line-forward nil)
  (setq sclang-runtime-directory "~/.sclang/")
  (setq sclang-help-path (quote ("/usr/share/SuperCollider/HelpSource")))
)

(abdo-supercollider-things)

(provide 'abdo-supercollider)
