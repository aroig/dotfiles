
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

