(provide 'abdo-zenburn)

;; NOTE: zenburn-with-color-variables is defined in the zenburn theme
;; from zenburn theme
; (defmacro zenburn-with-color-variables (&rest body)
;   "`let' bind all colors defined in `zenburn-colors-alist'.
; Also bind `class' to ((class color) (min-colors 89))."
;   (declare (indent 0))
;   `(let ((class '((class color) (min-colors 89)))
;          ,@(mapcar (lambda (cons)
;                     (list (intern (car cons)) (cdr cons)))
;                   zenburn-colors-alist))
;     ,@body))

(defun abdo-zenburn-things ()
;;;;; evil cursors
  (zenburn-with-color-variables
    (setq evil-default-cursor        `(,zenburn-fg box))
    (setq evil-normal-state-cursor   `(,zenburn-fg box))
    (setq evil-insert-state-cursor   `(,zenburn-sat-lightgreen bar))
    (setq evil-emacs-state-cursor    `(,zenburn-sat-yellow bar))
    (setq evil-motion-state-cursor   `(,zenburn-fg box))
    (setq evil-visual-state-cursor   `(,zenburn-fg box))
    (setq evil-replace-state-cursor  `(,zenburn-red-2 box))
    (setq evil-operator-state-cursor `(,zenburn-red-2 box)))

  (zenburn-with-color-variables
    (custom-set-faces
;;;;; abdo faces
    `(abdo-save-question ((t (:foreground ,zenburn-sat-lightgreen-1))))
    `(abdo-commit-question ((t (:foreground ,zenburn-sat-orange))))
;;;;; org
    `(org-priority-level-1 ((t (:foreground ,zenburn-sat-red-1))))
    `(org-priority-level-2 ((t (:foreground ,zenburn-sat-orange-1))))
    `(org-priority-level-3 ((t (:foreground ,zenburn-yellow-2))))
    `(org-priority-level-4 ((t (:foreground ,zenburn-green+1))))
    `(org-priority-level-5 ((t (:foreground ,zenburn-blue))))
;;;;; powerline
    `(powerline-active0      ((t (:foreground ,zenburn-yellow   :background ,zenburn-bg-1     :inherit mode-line))))
    `(powerline-active1      ((t (:foreground ,zenburn-green+1  :background ,zenburn-bg+1     :inherit mode-line))))
    `(powerline-active2      ((t (:foreground ,zenburn-blue     :background ,zenburn-bg+3     :inherit mode-line))))
    `(powerline-active-alert ((t (:foreground ,zenburn-red+1    :background ,zenburn-bg+3     :inherit mode-line))))

    `(powerline-inactive0      ((t (:foreground ,zenburn-green  :background ,zenburn-bg-05    :inherit mode-line))))
    `(powerline-inactive1      ((t (:foreground ,zenburn-green  :background ,zenburn-bg-05    :inherit mode-line))))
    `(powerline-inactive2      ((t (:foreground ,zenburn-green  :background ,zenburn-bg-05    :inherit mode-line))))
    `(powerline-inactive-alert ((t (:foreground ,zenburn-red+1  :background ,zenburn-bg-05    :inherit mode-line))))

    `(powerline-root0        ((t (:foreground ,zenburn-yellow   :background ,zenburn-root-1   :inherit mode-line))))
    `(powerline-root1        ((t (:foreground ,zenburn-green+1  :background ,zenburn-root     :inherit mode-line))))
    `(powerline-root2        ((t (:foreground ,zenburn-blue     :background ,zenburn-root+1   :inherit mode-line))))
    `(powerline-root-alert   ((t (:foreground ,zenburn-red+1    :background ,zenburn-root+1   :inherit mode-line))))

    `(powerline-evil-inactive  ((t (:foreground ,zenburn-green  :background ,zenburn-bg-05    :inherit mode-line))))
    `(powerline-evil-normal    ((t (:foreground ,zenburn-bg-1   :background ,zenburn-bg+4     :inherit mode-line))))
    `(powerline-evil-insert    ((t (:foreground ,zenburn-bg-1   :background ,zenburn-green    :inherit mode-line))))
    `(powerline-evil-visual    ((t (:foreground ,zenburn-bg-1   :background ,zenburn-blue+1   :inherit mode-line))))
    `(powerline-evil-operator  ((t (:foreground ,zenburn-bg-1   :background ,zenburn-red-1    :inherit mode-line))))
    `(powerline-evil-motion    ((t (:foreground ,zenburn-bg-1   :background ,zenburn-blue+1   :inherit mode-line))))
    `(powerline-evil-replace   ((t (:foreground ,zenburn-bg-1   :background ,zenburn-red-1    :inherit mode-line))))
    `(powerline-evil-emacs     ((t (:foreground ,zenburn-bg-1   :background ,zenburn-yellow-2 :inherit mode-line))))
    `(powerline-evil-unknown   ((t (:foreground ,zenburn-bg-1   :background ,zenburn-red-1    :inherit mode-line))))
    )))


(abdo-zenburn-things)
