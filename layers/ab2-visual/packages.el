(setq ab2-visual-packages
      '(
        spaceline
        diminish
        theming
        ))

(defun ab2-visual/post-init-spaceline ()
  ;; override default version-control segment

  ;; Override major mode segment
  (spaceline-define-segment major-mode
    "The name of the major mode."
    (downcase (powerline-major-mode)))

  ;; Override version-control segment
  (spaceline-define-segment version-control
    "Personalized version control information"
    (when vc-mode
      (powerline-raw
       (s-trim (concat
                (replace-regexp-in-string "Git." "⎇ " vc-mode)
                (when (buffer-file-name)
                  (pcase (vc-state (buffer-file-name))
                    (`up-to-date " ✓")
                    (`edited " *")
                    (`added " +")
                    (`unregistered " ?")
                    (`removed " -")
                    (`needs-merge " X")
                    (`needs-update " *")
                    (`ignored " ·")
                    (_ " ?"))))))))

  ;; Override buffer-position segment
  (spaceline-define-segment buffer-position
    "The current approximate buffer position, in percent."
    (powerline-raw
     (replace-regexp-in-string
      "%" "%%" (downcase (substring (format-mode-line "%p") 0 3)))))

  ;; Override buffer-encoding-abbrev segment
  (spaceline-define-segment buffer-encoding-abbrev
    "The line ending convention used in the buffer."
    (let ((buf-coding (format "%s" buffer-file-coding-system)))
      (when (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (let ((buf-newline (match-string 1 buf-coding)))
          (cond
           ((string= buf-newline "dos") (powerline-raw "w"))
           ((string= buf-newline "unix") (powerline-raw "u"))
           ((string= buf-newline "mac") (powerine-raw "m")))))))

  ;; Disable some segments
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-new-version-off)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-line-column-on))

(defun ab2-visual/post-init-diminish ()
  ;; diminish some more modes.
  ;; NOTE: I do not follow the convention that the letter corresponds with the key binding
  (spacemacs|diminish server-buffer-clients "ⓥ" "v")
  (spacemacs|diminish binary-overwrite-mode "Ⓞb" "Ob")
  (spacemacs|diminish overwrite-mode "Ⓞ" "O")
  (spacemacs|diminish isearch-mode "/" "/")
  )

;; NOTE: when changing this, run (spacemacs/update-theme)
(defun ab2-visual/pre-init-theming ()
  (setq
   theming-headings-inherit-from-default 'all
   theming-headings-same-size 'all)

  (ab2-visual/with-zenburn-color-variables
    (setq
     theming-modifications
     `((zenburn
        ;; Fringe and region
        (fringe :foreground ,zenburn-fg :background ,zenburn-bg)
        (region :background ,zenburn-green-1)
        ;; Flat boxes in the modeline
        (mode-line :foreground ,zenburn-green+1 :background ,zenburn-bg-05 :box (:color ,zenburn-fg-1))
        (mode-line-inactive :foreground ,zenburn-green+1 :background ,zenburn-bg-05 :box (:color ,zenburn-fg-1))
        (mode-line-highlight :box (:color ,zenburn-fg))
        ;; success / warning / error
        (success :foreground ,zenburn-green+2 :weight bold)
        (warning :foreground ,zenburn-yellow-1 :weight bold)
        (error :foreground ,zenburn-red :weight bold)
        ;; fonts
        (variable-pitch :inherit default :font-family nil)
        ;; cfw
        (cfw:face-today :foreground ,zenburn-yellow-1 :weight bold)
        ;; Company
        (company-echo-common :foreground ,zenburn-red)
        (company-preview :foreground ,zenburn-bg :background ,zenburn-green+2)
        (company-preview-search :background ,zenburn-blue-3)
        (company-template-field :foreground ,zenburn-bg :background ,zenburn-yellow-1)
        ;; diff
        (diff-added :background ,zenburn-dark-green :foreground ,zenburn-fg)
        (diff-changed :background ,zenburn-dark-yellow :foreground ,zenburn-fg)
        (diff-removed :background ,zenburn-dark-red :foreground ,zenburn-fg)
        (diff-refine-added :background ,zenburn-green-1 :foreground ,zenburn-fg)
        (diff-refine-change :backgroun ,zenburn-yellow-2 :foreground ,zenburn-fg)
        (diff-refine-removed :background ,zenburn-red-4 :foreground ,zenburn-fg)
        (diff-context :background ,zenburn-bg :foreground ,zenburn-fg)
        (diff-header :background ,zenburn-bg+2 :foreground ,zenburn-fg)
        (diff-file-header :background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold)
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
        ;; latex
        (font-latex-script-char-face :inherit font-latex-math-face)
        (font-latex-sectioning-1-face :inherit 'outline-1 :weight bold)
        (font-latex-sectioning-2-face :inherit 'outline-2 :weight bold)
        (font-latex-sectioning-3-face :inherit 'outline-3 :weight bold)
        (font-latex-sectioning-4-face :inherit 'outline-4 :weight bold)
        (font-latex-sectioning-5-face :inherit 'outline-5 :weight bold)
        ;; markdown
        (markdown-header-face-1 :inherit 'outline-1 :weight bold)
        (markdown-header-face-2 :inherit 'outline-2 :weight bold)
        (markdown-header-face-3 :inherit 'outline-3 :weight bold)
        (markdown-header-face-4 :inherit 'outline-4 :weight bold)
        (markdown-header-face-5 :inherit 'outline-5 :weight bold)
        (markdown-header-face-6 :inherit 'outline-6 :weight bold)
        ;; mu4e
        (mu4e-unread-face :foreground ,zenburn-orange+1 :weight bold)
        ;; persp
        (persp-face-lighter-buffer-not-in-persp :background ,zenburn-red :foreground ,zenburn-blue-4)
        ;; rtags
        (rtags-errline :background ,zenburn-red)
        (rtags-fixitline :background ,zenburn-orange-1)
        (rtags-warnline :background ,zenburn-blue-2)
        ;; smerge
        (smerge-base :inherit diff-context)
        (smerge-markers :inherit diff-file-header)
        (smerge-mine :inherit diff-added)
        (smerge-other :inherit diff-removed)
        (smerge-refined-added :inherit diff-refine-added)
        (smerge-refined-changed :inherit diff-refine-changed)
        (smerge-refined-removed :inherit diff-refine-removed)
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
        ;; vc
        (vc-conflict-state :inherit error :weight normal)
        (vc-edited-state :inherit warning :weight normal)
        (vc-locally-added-state :inherit warning :weight normal)
        (vc-locked-state :inherit warning :weight normal)
        (vc-missing-state :inherit warning :weight normal)
        (vc-needs-update-state :inherit warning :weight normal)
        (vc-removed-state :inherit warning :weight normal)
        (vc-state-base :inherit default :weight normal)
        (vc-up-to-date-state :inherit success :weight normal)
        ;; widget
        (widget-button-pressed :foreground ,zenburn-red-1)
        (widged-documentation :foreground ,zenburn-green+1)
        )))))
