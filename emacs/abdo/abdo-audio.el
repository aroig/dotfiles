

;; Supercollider
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-supercollider-things ()
  (setq sclang-auto-scroll-post-buffer t)
  (setq sclang-eval-line-forward nil)
  (setq sclang-runtime-directory "~/.sclang/")
  (setq sclang-help-path (quote ("/usr/share/SuperCollider/HelpSource")))
)
(add-hook 'sclang-mode-hook 'abdo-supercollider-things)



;; Extempore
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-extempore-things ()
  (extempore-connect "localhost" 7099)
)

(add-hook 'extempore-mode-hook 'abdo-extempore-things)


(abdo-supercollider-things)

(provide 'abdo-audio)
