

;; Yaml mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'yaml-mode-hook (lambda ()
                            (setq yaml-block-literal-electric-alist '())))


(provide 'abdo-text)
