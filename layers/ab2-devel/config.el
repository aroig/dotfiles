
;; compile buffer
(defvar ab2/compile-window-state nil)

;; systemd mode assignments
(add-to-list 'auto-mode-alist '("\\.\\(service\\|target\\|slice\\|timer\\|path\\|preset\\)$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.\\(link\\|network\\|netdev\\)$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.\\(mount\\|automount\\|socket\\|device\\)$" . conf-mode))


;; Basic prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil        ;; No tabs on indent
                          tab-width 4)
            ))

;; enable auto-fill on programming modes
;; (add-hook 'prog-mode-hook 'turn-on-auto-fill)

