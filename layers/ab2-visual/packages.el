(setq ab2-visual-packages
      '(
        spaceline
        diminish
        theming
        ))

(defun ab2-visual/pre-init-spaceline ()
  (setq
   spaceline-buffer-size-p nil
   spaceline-line-column t
   ))

(defun ab2-visual/post-init-diminish ()

  ;; diminish some more modes.
  ;; NOTE: I do not follow the convention that the letter corresponds with the key binding
  (spacemacs|diminish server-buffer-clients "ⓥ" "v")
  (spacemacs|diminish binary-overwrite-mode "Ⓞb" "O")
  (spacemacs|diminish overwrite-mode "Ⓞ" "O")
  (spacemacs|diminish isearch-mode "/" "/")
  )

(defun ab2-visual/pre-init-theming ()
  (setq
   theming-headings-inherit-from-default 'all
   theming-headings-same-size 'all
   theming-modifications '())

  (ab2-visual/with-zenburn-color-variables
    (add-to-list
     'theming-modifications
     `(zenburn
       ;; Fringe and region
       (fringe :foreground ,zenburn-fg :background ,zenburn-bg)
       (region :background ,zenburn-green-1)
       ;; Flat boxes in the modeline
       (mode-line :foreground ,zenburn-green+1 :background ,zenburn-bg-05 :box (:color ,zenburn-fg-1))
       (mode-line-inactive :foreground ,zenburn-green+1 :background ,zenburn-bg-05 :box (:color ,zenburn-fg-1))
       (mode-line-highlight :box (:color ,zenburn-fg))
       ;; Company
       (company-echo-common :foreground ,zenburn-red)
       (company-preview :foreground ,zenburn-bg :background ,zenburn-green+2)
       (company-preview-search :background ,zenburn-blue-3)
       (company-template-field :foreground ,zenburn-bg :background ,zenburn-yellow-1)
       ;; eval-sexp
       (eval-sexp-fu-flash :background ,zenburn-blue-5)
       (eval-sexp-fu-flash-error :foreground ,zenburn-red)
       ;; evil
       (evil-search-highlight-persist-highlight-face :inherit region)
       ;; helm
       (helm-header-line-left-margin :foreground ,zenburn-bg :background ,zenburn-yellow-1)
       (helm-resume-need-update :foreground ,zenburn-bg :background ,zenburn-red-1)
       (helm-source-header :weight bold :box nil :foreground ,zenburn-yellow-1 :background ,zenburn-bg-2)
       ;; hl
       (hl-line :background ,zenburn-bg+1)
       ;; markdown
       (markdown-header-face-1 :inherit 'outline-1 :weight bold)
       (markdown-header-face-2 :inherit 'outline-2 :weight bold)
       (markdown-header-face-3 :inherit 'outline-3 :weight bold)
       (markdown-header-face-4 :inherit 'outline-4 :weight bold)
       (markdown-header-face-5 :inherit 'outline-5 :weight bold)
       (markdown-header-face-6 :inherit 'outline-6 :weight bold)
       ;; mu4e
       (mu4e-unread-face :foreground ,zenburn-orange :weight bold)
       ;; persp
       (persp-face-lighter-buffer-not-in-persp :background ,zenburn-red :foreground ,zenburn-blue-4)
       ;; rtags
       (rtags-errline :background ,zenburn-red)
       (rtags-fixitline :background ,zenburn-orange-1)
       (rtags-warnline :background ,zenburn-blue-2)
       ;; spaceline
       (spaceline-highlight-face :inherit mode-line :foreground ,zenburn-bg :background ,zenburn-orange+1)
       (spaceline-evil-emacs :inherit mode-line :foreground ,zenburn-bg :background ,zenburn-blue-1)
       (spaceline-evil-insert :inherit mode-line :foreground ,zenburn-bg :background ,zenburn-green+1)
       (spaceline-evil-motion :inherit mode-line :foreground ,zenburn-bg :background ,zenburn-red+1)
       (spaceline-evil-normal :inherit mode-line :foreground ,zenburn-bg :background ,zenburn-yellow-1)
       (spaceline-evil-replace :inherit mode-line :foreground ,zenburn-bg :background ,zenburn-orange+1)
       (spaceline-evil-visual :inherit mode-line :foreground ,zenburn-bg :background ,zenburn-fg)
       ;; spacemacs
       (spacemacs-micro-state-header-face :foreground ,zenburn-bg :background ,zenburn-orange+1)
       (spacemacs-iedit-face :background ,zenburn-red-4 :inherit 'mode-line)
       (spacemacs-iedit-insert-face :background ,zenburn-red-4 :inherit 'mode-line)
       (spacemacs-highlight-face :foreground ,zenburn-bg :background ,zenburn-orange+1)
       (spacemacs-emacs-face :foreground ,zenburn-bg :background ,zenburn-blue-1)
       (spacemacs-insert-face :foreground ,zenburn-bg :background ,zenburn-green+1)
       (spacemacs-motion-face :foreground ,zenburn-bg :background ,zenburn-red+1)
       (spacemacs-normal-face :foreground ,zenburn-bg :background ,zenburn-yellow-1)
       (spacemacs-replace-face :foreground ,zenburn-bg :background ,zenburn-orange+1)
       (spacemacs-visual-face :foreground ,zenburn-bg :background ,zenburn-fg)
       (spacemacs-evilified-face :foreground ,zenburn-bg :background ,zenburn-green+1)
       ;; org
       (org-priority-level-1 :foreground ,zenburn-sat-red)
       (org-priority-level-2 :foreground ,zenburn-sat-orange-1)
       (org-priority-level-3 :foreground ,zenburn-yellow-2)
       (org-priority-level-4 :foreground ,zenburn-green+1)
       (org-priority-level-5 :foreground ,zenburn-blue)
       ;; widget
       (widget-button-pressed :foreground ,zenburn-red-1)
       (widged-documentation :foreground ,zenburn-green+1)
       ))))
