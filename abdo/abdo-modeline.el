(provide 'abdo-modeline)


;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface powerline-active0 '((t (:background "grey22" :inherit mode-line)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-active1 '((t (:background "grey22" :inherit mode-line)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-active2 '((t (:background "grey40" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-inactive0
  '((t (:background "grey11" :inherit mode-line-inactive)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-inactive1
  '((t (:background "grey11" :inherit mode-line-inactive)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-inactive2
  '((t (:background "grey20" :inherit mode-line-inactive)))
  "Powerline face 2."
  :group 'powerline)


(defface powerline-evil-inactive '((t (:background "black" :inherit mode-line)))
  "Powerline evil inactive state face."
  :group 'powerline)

(defface powerline-evil-normal '((t (:background "black" :inherit mode-line)))
  "Powerline evil normal state face."
  :group 'powerline)

(defface powerline-evil-insert '((t (:background "green" :inherit mode-line)))
  "Powerline evil insert state face."
  :group 'powerline)

(defface powerline-evil-visual '((t (:background "black" :inherit mode-line)))
  "Powerline evil visual state face."
  :group 'powerline)

(defface powerline-evil-operator '((t (:background "red" :inherit mode-line)))
  "Powerline evil operator state face."
  :group 'powerline)

(defface powerline-evil-motion '((t (:background "black" :inherit mode-line)))
  "Powerline evil motion state face."
  :group 'powerline)

(defface powerline-evil-replace '((t (:background "red" :inherit mode-line)))
  "Powerline evil replace state face."
  :group 'powerline)

(defface powerline-evil-emacs '((t (:background "yellow" :inherit mode-line)))
  "Powerline evil emacs state face."
  :group 'powerline)

(defface powerline-evil-unknown '((t (:background "red" :inherit mode-line)))
  "Powerline evil unknown state face."
  :group 'powerline)



;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-evil-state ()
  (cond ((evil-normal-state-p)   "N")
        ((evil-insert-state-p)   "I")
        ((evil-visual-state-p)   "V")
        ((evil-operator-state-p) "O")
        ((evil-motion-state-p)   "M")
        ((evil-replace-state-p)  "R")
        ((evil-emacs-state-p)    "E")
        (t                       "U")))


(defun abdo-evil-face (&optional state)
    ;; Don't propertize if we're not in the selected buffer
    (cond ;((not (eq (current-buffer) (car (buffer-list)))) 'powerline-evil-inactive)
          ((evil-normal-state-p)   'powerline-evil-normal)
          ((evil-insert-state-p)   'powerline-evil-insert)
          ((evil-visual-state-p)   'powerline-evil-visual)
          ((evil-operator-state-p) 'powerline-evil-operator)
          ((evil-motion-state-p)   'powerline-evil-motion)
          ((evil-replace-state-p)  'powerline-evil-replace)
          ((evil-emacs-state-p)    'powerline-evil-emacs)
          (t                       'powerline-evil-unknown)))


(defun powerline-mode-list (face)
  (let ((major (propertize
                 (format-mode-line
                  (cond
                   ((listp mode-name) (downcase (car mode-name)))
                   ((stringp mode-name) (downcase mode-name))))

           'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"

           'local-map (let ((map (make-sparse-keymap)))
                        (define-key map [mode-line down-mouse-1]
                          `(menu-item ,(purecopy "Menu Bar") ignore
                                      :filter (lambda (_) (mouse-menu-major-mode-map))))
                        (define-key map [mode-line mouse-2]      'describe-mode)
                        (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                        map)))

        (minor (mapconcat (lambda (mm) (propertize (downcase mm)

           'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"

           'local-map (let ((map (make-sparse-keymap)))
                        (define-key map [mode-line down-mouse-1]   (powerline-mouse 'minor 'menu mm))
                        (define-key map [mode-line mouse-2]        (powerline-mouse 'minor 'help mm))
                        (define-key map [mode-line down-mouse-3]   (powerline-mouse 'minor 'menu mm))
                        (define-key map [header-line down-mouse-3] (powerline-mouse 'minor 'menu mm))
                        map)))
                          (split-string (format-mode-line minor-mode-alist)) " ")))

    (if (not (string= minor ""))
        (propertize (concat " " major " | " minor) 'face face)
      (propertize (concat " " major) 'face face))))




;; Modeline tweaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-modeline-things ()
  (setq eol-mnemonic-unix      "x")
  (setq eol-mnemonic-dos       "d")
  (setq eol-mnemonic-mac       "m")
  (setq eol-mnemonic-undecided "?")
)


;; Powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/PowerLine



(defun abdo-powerline-things ()
  (require 'powerline)
  (setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((active    (eq (frame-selected-window) (selected-window)))
                        (face0     (if active 'powerline-active0 'powerline-inactive0))
                        (face1     (if active 'powerline-active1 'powerline-inactive1))
                        (face2     (if active 'powerline-active2 'powerline-inactive2))
                        (evilstate (abdo-evil-state))
                        (evilface  (if active (abdo-evil-face evilstate) 'powerline-evil-inactive))
                        (lhs (list
                              ; evil state
                              (propertize (format " %s " evilstate) 'face evilface)
                              (powerline-arrow-right evilface face0)

                              ; buffer id
                              (powerline-raw "%b " face0 'l)
                              (powerline-arrow-right face0 face1)

                              ; modes
                              (powerline-mode-list face1)
                              (powerline-raw mode-line-process face1 'l)
                              (powerline-arrow-right face1 face2)

                              ; vcs
                              (powerline-vc face2)
                              ))

                        (rhs (list
                              ; mode string
                              (powerline-raw global-mode-string face2 'r)
                              (powerline-arrow-left face2 face1)

                              ; position
                              (powerline-raw " %l:%2c %p" face1 'r)
                              (powerline-narrow face1 'r)
                              (powerline-arrow-left face1 face0)

                              ; state
                              (powerline-raw " %*%Z " face0 'r)
                              (powerline-hud face2 face1)
                              )))

                   (concat
                    (powerline-render lhs)
                    (powerline-fill face2 (powerline-width rhs))
                    (powerline-render rhs))))))
)


;; Tweaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(abdo-modeline-things)
(abdo-powerline-things)
