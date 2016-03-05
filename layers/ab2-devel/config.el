

;; systemd mode assignments
(add-to-list 'auto-mode-alist '("\\.\\(service\\|target\\|slice\\|timer\\|path\\|preset\\)$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.\\(link\\|network\\|netdev\\)$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.\\(mount\\|automount\\|socket\\|device\\)$" . conf-mode))


;; TODO: do I want it?
;; enable auto-fill on programming modes
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

