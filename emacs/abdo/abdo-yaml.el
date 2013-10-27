

;; Custom set stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yaml-mode-things ()
  (setq yaml-block-literal-electric-alist '()))

(add-hook 'yaml-mode-hook 'yaml-mode-things)
(provide 'abdo-yaml)
