
  ;; tweak zenburn colors
(defvar zenburn-colors-alist
  '(("zenburn-fg+1"     . "#FFFFEF")
    ("zenburn-fg"       . "#DCDCCC")
    ("zenburn-fg-1"     . "#656555")
    ("zenburn-bg-2"     . "#000000")
    ("zenburn-bg-1"     . "#1B1B1B")
    ("zenburn-bg-05"    . "#282828")
    ("zenburn-bg"       . "#2F2F2F")
    ("zenburn-bg+05"    . "#393939")
    ("zenburn-bg+1"     . "#3F3F3F")
    ("zenburn-bg+2"     . "#4F4F4F")
    ("zenburn-bg+3"     . "#5F5F5F")
    ("zenburn-red+1"    . "#DCA3A3")
    ("zenburn-red"      . "#CC9393")
    ("zenburn-red-1"    . "#BC8383")
    ("zenburn-red-2"    . "#AC7373")
    ("zenburn-red-3"    . "#9C6363")
    ("zenburn-red-4"    . "#8C5353")
    ("zenburn-orange+1" . "#EfBF9F")
    ("zenburn-orange"   . "#DFAF8F")
    ("zenburn-orange-1" . "#CF9F7F")
    ("zenburn-orange-2" . "#BF8F6F")
    ("zenburn-yellow"   . "#F0DFAF")
    ("zenburn-yellow-1" . "#E0CF9F")
    ("zenburn-yellow-2" . "#D0BF8F")
    ("zenburn-green-1"  . "#5F7F5F")
    ("zenburn-green"    . "#7F9F7F")
    ("zenburn-green+1"  . "#8FB28F")
    ("zenburn-green+2"  . "#9FC59F")
    ("zenburn-green+3"  . "#AFD8AF")
    ("zenburn-green+4"  . "#BFEBBF")
    ("zenburn-cyan"     . "#93E0E3")
    ("zenburn-blue+1"   . "#94BFF3")
    ("zenburn-blue"     . "#8CD0D3")
    ("zenburn-blue-1"   . "#7CB8BB")
    ("zenburn-blue-2"   . "#6CA0A3")
    ("zenburn-blue-3"   . "#5C888B")
    ("zenburn-blue-4"   . "#4C7073")
    ("zenburn-blue-5"   . "#366060")
    ("zenburn-magenta"  . "#DC8CC3")

    ;; saturated colors
    ("zenburn-sat-yellow"        . "#FFE241")
    ("zenburn-sat-yellow-1"      . "#E1BB37")
    ("zenburn-sat-orange"        . "#FF8F35")
    ("zenburn-sat-orange-1"      . "#DD7621")
    ("zenburn-sat-red"           . "#D55252")
    ("zenburn-sat-red-1"         . "#B53232")
    ("zenburn-sat-red-2"         . "#850202")
    ("zenburn-sat-green"         . "#00CD66")  ;; SpringGreen3
    ("zenburn-sat-green-1"       . "#008B45")  ;; SpringGreen4
    ("zenburn-sat-lightgreen"    . "#9AFF9A")  ;; PaleGreen1
    ("zenburn-sat-lightgreen-1"  . "#7CCD7C")  ;; PaleGreen3
    ("zenburn-sat-blue"          . "#67B1F6")
    ("zenburn-sat-blue-1"        . "#448CD0")
    ("zenburn-sat-purple"        . "#9B30FF")  ;; purple1

    ;; dark colors
    ("zenburn-dark-brown"        . "#533319")
    ("zenburn-dark-green"        . "#2D5842")
    ("zenburn-dark-red"          . "#681313")
    ("zenburn-dark-blue"         . "#2F4276")
    ))


(defun ab2/zenburn-pre-tweak (theme)
  ;; zenburn theme
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     ;; get rid of bringes and boxes
     `(fringe ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
     `(mode-line ((,class (:foreground ,zenburn-green+1 :background ,zenburn-bg-05 :box (:color ,zenburn-fg-1)))
                  (t :inverse-video t)))
     `(mode-line-inactive ((,class (:foreground ,zenburn-green+1 :background ,zenburn-bg-05 :box (:color ,zenburn-fg-1)))
                           (t :inverse-video t)))
     `(mode-line-highlight ((t (:box (:color ,zenburn-fg)))))
     ))
  )


(defun ab2/zenburn-post-tweak (theme)
  ;; zenburn theme
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     ;; evil
     `(evil-search-highlight-persist-highlight-face ((t (:inherit region))))
     ;; helm
     `(helm-header-line-left-margin ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-1))))
     `(helm-resume-need-update ((t (:foreground ,zenburn-bg :background ,zenburn-red-1))))
     `(helm-source-header ((t (:weight bold :box nil :foreground ,zenburn-yellow-1 :background ,zenburn-bg-2))))
     ;; spaceline
     `(spaceline-highlight-face ((t (:inherit mode-line :foreground ,zenburn-bg :background ,zenburn-orange+1))))
     `(spaceline-evil-emacs ((t (:inherit mode-line :foreground ,zenburn-bg :background ,zenburn-blue-1))))
     `(spaceline-evil-insert ((t (:inherit mode-line :foreground ,zenburn-bg :background ,zenburn-green+1))))
     `(spaceline-evil-motion ((t (:inherit mode-line :foreground ,zenburn-bg :background ,zenburn-red+1))))
     `(spaceline-evil-normal ((t (:inherit mode-line :foreground ,zenburn-bg :background ,zenburn-yellow-1))))
     `(spaceline-evil-replace ((t (:inherit mode-line :foreground ,zenburn-bg :background ,zenburn-orange+1))))
     `(spaceline-evil-visual ((t (:inherit mode-line :foreground ,zenburn-bg :background ,zenburn-fg))))
     ;; spacemacs
     `(spacemacs-highlight-face ((t (:foreground ,zenburn-bg :background ,zenburn-orange+1))))
     `(spacemacs-emacs-face ((t (:foreground ,zenburn-bg :background ,zenburn-blue-1))))
     `(spacemacs-insert-face ((t (:foreground ,zenburn-bg :background ,zenburn-green+1))))
     `(spacemacs-motion-face ((t (:foreground ,zenburn-bg :background ,zenburn-red+1))))
     `(spacemacs-normal-face ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-1))))
     `(spacemacs-replace-face ((t (:foreground ,zenburn-bg :background ,zenburn-orange+1))))
     `(spacemacs-visual-face ((t (:foreground ,zenburn-bg :background ,zenburn-fg))))
     `(spacemacs-evilified-face ((t (:foreground ,zenburn-bg :background ,zenburn-green+1))))
     ))
  )

