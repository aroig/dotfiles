
;; Color Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'abdo-colors)

;; TODO: Enable hooks to add color tweaks in other places.

;; General Zenburn tweaks
(defun zenburn-color-tweaks()
  (interactive)

  ;; Zenburn colors with darker background
  (setq zenburn-fg "#dcdccc")
  (setq zenburn-fg-1 "#656555")
  (setq zenburn-bg-1 "#1b1b1b")
  (setq zenburn-bg-05 "#282828")
  (setq zenburn-bg "#2f2f2f")
  (setq zenburn-bg+1 "#3f3f3f")
  (setq zenburn-bg+2 "#4f4f4f")
  (setq zenburn-bg+3 "#5f5f5f")
  (setq zenburn-red+1 "#dca3a3")
  (setq zenburn-red "#cc9393")
  (setq zenburn-red-1 "#bc8383")
  (setq zenburn-red-2 "#ac7373")
  (setq zenburn-red-3 "#9c6363")
  (setq zenburn-red-4 "#8c5353")
  (setq zenburn-orange "#dfaf8f")
  (setq zenburn-yellow "#f0dfaf")
  (setq zenburn-yellow-1 "#e0cf9f")
  (setq zenburn-yellow-2 "#d0bf8f")
  (setq zenburn-green-1 "#5f7f5f")
  (setq zenburn-green "#7f9f7f")
  (setq zenburn-green+1 "#8fb28f")
  (setq zenburn-green+2 "#9fc59f")
  (setq zenburn-green+3 "#afd8af")
  (setq zenburn-green+4 "#bfebbf")
  (setq zenburn-cyan "#93e0e3")
  (setq zenburn-blue+1 "#94bff3")
  (setq zenburn-blue "#8cd0d3")
  (setq zenburn-blue-1 "#7cb8bb")
  (setq zenburn-blue-2 "#6ca0a3")
  (setq zenburn-blue-3 "#5c888b")
  (setq zenburn-blue-4 "#4c7073")
  (setq zenburn-blue-5 "#366060")
  (setq zenburn-magenta "#dc8cc3")


  ;; More tones of zenburn colors
  (setq zenburn-orange-1 "#cf9f7f")
  (setq zenburn-orange+1 "#efbf9f")

  (setq zenburn-fg-1     "#acac9c")
  (setq zenburn-fg-2     "#656555")

  (setq zenburn-wood     "#cdaa7d")             ;; burlywood3

  ;; More saturated colors
  (setq zenburn-sat-yellow       "#ffe241")     ;;
  (setq zenburn-sat-yellow-1     "#e1bb37")     ;;
  (setq zenburn-sat-orange       "#ff7f41")     ;; DarkOrange1
  (setq zenburn-sat-orange-1     "#dd7621")     ;; DarkOrange3
  (setq zenburn-sat-red          "#ee3b3b")     ;; brown1
  (setq zenburn-sat-red-1        "#cd3333")     ;; brown3
  (setq zenburn-sat-green        "#00cd66")     ;; SpringGreen3
  (setq zenburn-sat-green-1      "#008b45")     ;; SpringGreen4
  (setq zenburn-sat-lightgreen   "#9aff9a")     ;; PaleGreen1
  (setq zenburn-sat-lightgreen-1 "#7ccd7c")     ;; PaleGreen3
  (setq zenburn-sat-blue         "#1e90ff")     ;; DodgerBlue1
  (setq zenburn-sat-blue-1       "#3a5fcd")     ;; RoyalBlue3
  (setq zenburn-sat-purple       "#9b30ff")     ;; purple1


  ;; Dark colors
  (setq zenburn-dark-brown       "#533319")
  (setq zenburn-dark-green       "#2d5842")
  (setq zenburn-dark-red         "#681313")
  (setq zenburn-dark-blue        "#2f4276")


  ;; Terminal colors. Not using them. term colors are the ones of the system.
  (setq zenburn-term-0           "#2F2F2F")
  (setq zenburn-term-1           "#8F4F4F")
  (setq zenburn-term-2           "#60B48A")
  (setq zenburn-term-3           "#DFAF8F")
  (setq zenburn-term-4           "#577A9D")
  (setq zenburn-term-5           "#C57FAF")
  (setq zenburn-term-6           "#7AB1B4")
  (setq zenburn-term-7           "#DCDCCC")
  (setq zenburn-term-8           "#709080")
  (setq zenburn-term-9           "#DCA3A3")
  (setq zenburn-term-10          "#B5CEA0")
  (setq zenburn-term-11          "#F0DFAF")
  (setq zenburn-term-12          "#94BFF3")
  (setq zenburn-term-13          "#E9A0D4")
  (setq zenburn-term-14          "#A1DCDF")
  (setq zenburn-term-15          "#EEEEEC")

  (setq zenburn-term-bg          zenburn-term-0)
  (setq zenburn-term-fg          zenburn-term-15)

  ;; Get rid of the fringe
  (set-face-background 'fringe zenburn-bg)
)


(defun zenburn-root-tweaks()
  (set-face-background 'mode-line "#4b0b0b")
  (set-face-background 'region "#4b0b0b")
)

;; Applying tweaks and loading the color theme
(zenburn-color-tweaks)

;; Apply root colors if it is root
(let ((username (substring (shell-command-to-string "id -n -u") 0 -1)))
  (when (string= username "root") (zenburn-root-tweaks)))

;; Enable theme
; (color-theme-zenburn)

;; Enable dark mode.
(set-variable 'frame-background-mode 'dark)


;; flyspell
(custom-set-faces
  `(flyspell-duplicate ((t (:foreground ,zenburn-red-1))))
  `(flyspell-incorrect ((t (:foreground ,zenburn-red-1))))
)


;; org
(custom-set-faces
  `(org-tag ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))

  `(org-level-1 ((t (:foreground ,zenburn-sat-orange-1 :background ,zenburn-bg))))
  `(org-level-2 ((t (:foreground ,zenburn-sat-lightgreen-1 :background ,zenburn-bg))))
  `(org-level-3 ((t (:foreground ,zenburn-blue-1 :background ,zenburn-bg))))
  `(org-level-4 ((t (:foreground ,zenburn-yellow-2 :background ,zenburn-bg))))
  `(org-level-5 ((t (:foreground ,zenburn-green-1 :background ,zenburn-bg))))
  `(org-level-6 ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
  `(org-level-7 ((t (:foreground ,zenburn-red-4 :background ,zenburn-bg))))
  `(org-level-8 ((t (:foreground ,zenburn-blue-4 :background ,zenburn-bg))))
)


;; ido colors
(custom-set-faces
 `(link ((t (:foreground ,zenburn-blue))))
 `(ido-subdir ((t (:foreground ,zenburn-yellow))))
 `(ido-first-match ((t (:weight bold :foreground ,zenburn-red)))) ; highlight first match.
 `(ido-only-match ((t (:weight bold :foreground ,zenburn-red))))  ; highlight only match.
;;   '(ido-indicator ((t (:foreground "#ffffff"))))    ; highlight its indicators
;;   '(ido-incomplete-regexp ((t (:foreground "#ffffff")))) ; incomplete regexps
 )




;; ediff colors
(custom-set-faces
 `(ediff-current-diff-A ((t (:background ,zenburn-dark-red :foreground ,zenburn-fg))))
 `(ediff-current-diff-Ancestor ((t (:background ,zenburn-dark-red :foreground ,zenburn-fg))))
 `(ediff-current-diff-B ((t (:background ,zenburn-dark-red :foreground ,zenburn-fg))))
 `(ediff-current-diff-C ((t (:background ,zenburn-dark-red :foreground ,zenburn-fg))))

 `(ediff-even-diff-A ((t (:background ,zenburn-bg+2))))
 `(ediff-even-diff-Ancestor ((t (:background ,zenburn-bg+2))))
 `(ediff-even-diff-B ((t (:background ,zenburn-bg+2))))
 `(ediff-even-diff-C ((t (:background ,zenburn-bg+2))))

 `(ediff-odd-diff-A ((t (:background ,zenburn-bg+2))))
 `(ediff-odd-diff-Ancestor ((t (:background ,zenburn-bg+2))))
 `(ediff-odd-diff-B ((t (:background ,zenburn-bg+2))))
 `(ediff-odd-diff-C ((t (:background ,zenburn-bg+2))))

 `(ediff-fine-diff-A ((t (:background ,zenburn-dark-green))))
 `(ediff-fine-diff-Ancestor ((t (:background ,zenburn-dark-green :foreground ,zenburn-fg))))
 `(ediff-fine-diff-B ((t (:background ,zenburn-dark-green :foreground ,zenburn-fg))))
 `(ediff-fine-diff-C ((t (:background ,zenburn-dark-green :foreground ,zenburn-fg))))
)


;; notmuch
(custom-set-faces
  `(notmuch-crypto-decryption ((t (:background ,zenburn-yellow :foreground ,zenburn-bg-1))))
  `(notmuch-crypto-part-header ((t (:background ,zenburn-bg :foreground ,zenburn-blue-1))))
  `(notmuch-crypto-signature-bad ((t (:background ,zenburn-red :foreground ,zenburn-bg-1))))
  `(notmuch-crypto-signature-good  ((t (:background ,zenburn-green :foreground ,zenburn-bg-1))))
  `(notmuch-crypto-signature-good-key ((t (:background ,zenburn-yellow :foreground ,zenburn-bg-1))))
  `(notmuch-crypto-signature-unknown ((t (:background ,zenburn-red :foreground ,zenburn-bg-1))))
  `(notmuch-hello-logo-background ((t (:background ,zenburn-bg :foreground ,zenburn-fg))))

  `(notmuch-hello-logo-background ((t (:background ,zenburn-bg :foreground ,zenburn-fg))))
  `(notmuch-tag-face ((t (:background ,zenburn-bg :foreground "OliveDrab1"))))

  `(notmuch-search-count ((t (:background ,zenburn-bg :foreground ,zenburn-sat-lightgreen))))
  `(notmuch-search-date ((t (:background ,zenburn-bg :foreground ,zenburn-blue))))

;notmuch-message-summary-face
;notmuch-search-matching-authors
;notmuch-search-non-matching-authors
;notmuch-search-subject

)


;; Older Zenburn. Slightly different.
;;(require 'color-theme)
;;(color-theme-initialize)
;;(color-theme-zenburn)

;; Other themes I tried
;;(color-theme-abdo)
;;(color-theme-andreas)
;;(color-theme-blippblopp)
;;(color-theme-subtle-hacker)
;;(color-theme-arjen)
;;(color-theme-calm-forest)
;;(color-theme-dark-laptop)
;;(color-theme-late-night)
;;(color-theme-comidia)


;; (defun general-color-tweaks()
;;   (set-background-color "gray12")
;; )
