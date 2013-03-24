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

(defface powerline-active-alert
  '((t (:foreground "red" :inherit mode-line-active2)))
  "Powerline alert"
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

(defface powerline-inactive-alert
  '((t (:foreground "red" :inherit mode-line-inactive2)))
  "Powerline alert"
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



;; Powerline fragments
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

(defun abdo-powerline-evil-state ()
  (let* ((active (eq (frame-selected-window) (selected-window)))
         (face0 (if active 'powerline-active0 'powerline-inactive0))
         (evilstate (abdo-evil-state))
         (evilface (cond
                    ((not active)            'powerline-evil-inactive)
                    ((evil-normal-state-p)   'powerline-evil-normal)
                    ((evil-insert-state-p)   'powerline-evil-insert)
                    ((evil-visual-state-p)   'powerline-evil-visual)
                    ((evil-operator-state-p) 'powerline-evil-operator)
                    ((evil-motion-state-p)   'powerline-evil-motion)
                    ((evil-replace-state-p)  'powerline-evil-replace)
                    ((evil-emacs-state-p)    'powerline-evil-emacs)
                    (t                       'powerline-evil-unknown))))
    (powerline-render
     (list
      (propertize (format " %s " evilstate) 'face evilface)
      (powerline-arrow-right evilface face0)))))


(defun abdo-powerline-buffer-name ()
  (let* ((active (eq (frame-selected-window) (selected-window)))
         (face0 (if active 'powerline-active0 'powerline-inactive0))
         (face1 (if active 'powerline-active1 'powerline-inactive1)))
    (powerline-render
     (list
      (powerline-raw "%b " face0 'l)
      (powerline-arrow-right face0 face1)))))


(defun abdo-powerline-mode-list ()
  (let* ((active (eq (frame-selected-window) (selected-window)))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (major (propertize
                 (downcase (format-mode-line mode-name))

           'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"

           'local-map (let ((map (make-sparse-keymap)))
                        (define-key map [mode-line down-mouse-1]
                          `(menu-item ,(purecopy "Menu Bar") ignore
                                      :filter (lambda (_) (mouse-menu-major-mode-map))))
                        (define-key map [mode-line mouse-2]      'describe-mode)
                        (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                        map)))

        (minor (mapconcat
                (lambda (mm)
                  (when mm
                    (propertize
                     (downcase mm)
                     'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"

                     'local-map (let ((map (make-sparse-keymap)))
                                  (define-key map [mode-line down-mouse-1]   (powerline-mouse 'minor 'menu mm))
                                  (define-key map [mode-line mouse-2]        (powerline-mouse 'minor 'help mm))
                                  (define-key map [mode-line down-mouse-3]   (powerline-mouse 'minor 'menu mm))
                                  (define-key map [header-line down-mouse-3] (powerline-mouse 'minor 'menu mm))
                                  map))))
                (split-string (format-mode-line minor-mode-alist)) " ")))

    (powerline-render
     (list
      (if (not (string= minor ""))
          (propertize (concat " " major " | " minor " ") 'face face1)
        (propertize (concat " " major " ") 'face face1))
      (powerline-arrow-right face1 face2)))))


(defun abdo-powerline-middle ()
  (let* ((active (eq (frame-selected-window) (selected-window)))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (facealert (if active 'powerline-active-alert 'powerline-inactive-alert)))
    (powerline-render
     (list
      (when (and (buffer-file-name (current-buffer)) vc-mode)
        (powerline-raw (format "%s " (format-mode-line '(vc-mode vc-mode))) face2 'l))

      (when (> (length abdo-modeline-buffer-alert-list) 0)
        (powerline-raw (format "%s " (abdo-powerline-buffer-alerts)) facealert 'l))

      (when mode-line-process
        (powerline-raw (format "%s " mode-line-process) face2 'l))

      (when global-mode-string
        (powerline-raw (format-mode-line '(global-mode-string global-mode-string)) face2 'r))



      (powerline-fill face2 25)     ; everything on the right is fixed width
      ;; TODO: truncate this if it gets too long
     ))))


(defun abdo-powerline-position ()
  (let* ((active (eq (frame-selected-window) (selected-window)))
         (face0 (if active 'powerline-active0 'powerline-inactive0))
         (face1 (if active 'powerline-active1 'powerline-inactive1)))
    (powerline-render
     (list
      (powerline-arrow-left face1 face0)
      (powerline-raw "%5l %3c  %p" face0 'r)
      (powerline-narrow face0 'r)))))


(defun abdo-powerline-state ()
  (let* ((active (eq (frame-selected-window) (selected-window)))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2)))
    (powerline-render
     (list
      (powerline-arrow-left face2 face1)
      (powerline-raw " %*%Z" face1 'r)))))




;; Buffer alerts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-powerline-buffer-alerts ()
  (abdo-modeline-buffer-visit (current-buffer))
  (let ((alert-list abdo-modeline-buffer-alert-list))
    (mapconcat (lambda (buf) (buffer-name buf)) alert-list " ")))


(defvar abdo-modeline-buffer-alert-list  '()
  "List of buffers in need of attention")

(defun abdo-modeline-buffer-alert (buffer-or-name)
  (interactive)
  (add-to-list 'abdo-modeline-buffer-alert-list (get-buffer buffer-or-name)))

(defun abdo-modeline-buffer-visit (buffer-or-name)
  (setq abdo-modeline-buffer-alert-list
        (or (delete (get-buffer buffer-or-name) abdo-modeline-buffer-alert-list) '())))

(add-hook 'kill-buffer-hook (lambda () (abdo-modeline-buffer-visit (current-buffer))))


(defun abdo-modeline-truncate-to (str-val maxlen &optional ellipsis)
  "Truncate STRING to MAXLEN.

The returned value is of length MAXLEN or less, including
ELLIPSIS.

ELLIPSIS is a string inserted wherever characters were removed.
It defaults to the UCS character \"Horizontal Ellipsis\", or
\"...\" if extended characters are not displayable."
  ;; character x2026 = Horizontal Ellipsis
  (callf or ellipsis (if (char-displayable-p (decode-char 'ucs #x2026)) (string (decode-char 'ucs #x2026)) "..."))
  (when (> (length str-val) maxlen)
    (if (>= (length ellipsis) maxlen)
        (setq str-val ellipsis)
      (callf substring str-val 0 (- maxlen (length ellipsis)))
      (callf concat str-val ellipsis))
    (callf substring str-val 0 maxlen))
  str-val)

;; Modeline tweaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-modeline-things ()
  (interactive)
  (setq eol-mnemonic-unix      "x")
  (setq eol-mnemonic-dos       "d")
  (setq eol-mnemonic-mac       "m")
  (setq eol-mnemonic-undecided "?")

  (abdo-powerline-things))


;; Powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/PowerLine

(defun abdo-powerline-things ()
  (require 'powerline)

  (setq-default mode-line-format
                (list
                 '(:eval (abdo-powerline-evil-state))       ; evil state
                 '(:eval (abdo-powerline-buffer-name))      ; buffer name
                 '(:eval (abdo-powerline-mode-list))        ; mode list
                 '(:eval (abdo-powerline-middle))
                 '(:eval (abdo-powerline-state))            ; state
                 '(-15 (:eval (abdo-powerline-position)))    ; position
                 )))

;; Tweaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(abdo-modeline-things)
