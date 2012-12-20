(provide 'abdo-powerline)


;; Powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/PowerLine

;; Faces for evil state

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
    (cond ((not (eq (current-buffer) (car (buffer-list)))) 'mode-line)
          ((evil-normal-state-p)   'powerline-evil-normal)
          ((evil-insert-state-p)   'powerline-evil-insert)
          ((evil-visual-state-p)   'powerline-evil-visual)
          ((evil-operator-state-p) 'powerline-evil-operator)
          ((evil-motion-state-p)   'powerline-evil-motion)
          ((evil-replace-state-p)  'powerline-evil-replace)
          ((evil-emacs-state-p)    'powerline-evil-emacs)
          (t                       'powerline-evil-unknown)))



(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((active    (eq (frame-selected-window) (selected-window)))
                        (face1     (if active 'powerline-active1 'powerline-inactive1))
                        (face2     (if active 'powerline-active2 'powerline-inactive2))
                        (evilstate (abdo-evil-state))
                        (evilface  (abdo-evil-face evilstate))
                        (lhs (list

                              ; evil state
                              (propertize (format " %s " evilstate) 'face evilface)
                              (powerline-arrow-right evilface nil)

                              (powerline-raw "%*" nil 'l)
;                              (powerline-buffer-size nil 'l)
                              (powerline-buffer-id nil 'l)

                              (powerline-raw " ")
                              (powerline-arrow-right nil face1)

                              (powerline-major-mode face1 'l)
                              (powerline-minor-modes face1 'l)
                              (powerline-raw mode-line-process face1 'l)

                              (powerline-narrow face1 'l)

                              (powerline-arrow-right face1 face2)

                              (powerline-vc face2)
                              ))
                        (rhs (list
                              (powerline-raw global-mode-string face2 'r)

                              (powerline-arrow-left face2 face1)

                              (powerline-raw "%l:%c" face1 'r)
;                              (powerline-raw ":" face1)
;                              (powerline-raw "%c" face1 'r)

                              (powerline-arrow-left face1 nil)
                              (powerline-raw " ")

                              (powerline-raw "%7p" nil 'r)

                              (powerline-hud face2 face1))))
                   (concat
                    (powerline-render lhs)
                    (powerline-fill face2 (powerline-width rhs))
                    (powerline-render rhs))))))
