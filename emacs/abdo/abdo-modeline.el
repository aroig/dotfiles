(provide 'abdo-modeline)


;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar abdo-modeline-skip-modes-regexp "undo-tree\\|fill\\|server"
  "Regexp matching minor modes I want to hide from the modeline")

(defvar abdo-powerline-right-width 22
  "With of the right block of the powerline")


;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface powerline-active0 '((t (:background "grey22" :inherit mode-line)))
  "Powerline face 1." :group 'powerline)

(defface powerline-active1 '((t (:background "grey22" :inherit mode-line)))
  "Powerline face 1." :group 'powerline)

(defface powerline-active2 '((t (:background "grey40" :inherit mode-line)))
  "Powerline face 2." :group 'powerline)

(defface powerline-active-alert '((t (:foreground "red" :inherit powerline-active2)))
  "Powerline alert" :group 'powerline)


(defface powerline-root0 '((t (:inherit powerline-active0)))
  "Powerline face 1." :group 'powerline)

(defface powerline-root1 '((t (:inherit powerline-active1)))
  "Powerline face 1." :group 'powerline)

(defface powerline-root2 '((t (:inherit powerline-active2)))
  "Powerline face 2." :group 'powerline)

(defface powerline-root-alert '((t (:inherit powerline-active-alert)))
  "Powerline alert" :group 'powerline)


(defface powerline-inactive0 '((t (:background "grey11" :inherit mode-line-inactive)))
  "Powerline face 1." :group 'powerline)

(defface powerline-inactive1 '((t (:background "grey11" :inherit mode-line-inactive)))
  "Powerline face 1." :group 'powerline)

(defface powerline-inactive2 '((t (:background "grey20" :inherit mode-line-inactive)))
  "Powerline face 2." :group 'powerline)

(defface powerline-inactive-alert '((t (:foreground "red" :inherit powerline-inactive2)))
  "Powerline alert" :group 'powerline)


(defface powerline-evil-inactive '((t (:background "black" :inherit mode-line)))
  "Powerline evil inactive state face." :group 'powerline)

(defface powerline-evil-normal '((t (:background "black" :inherit mode-line)))
  "Powerline evil normal state face." :group 'powerline)

(defface powerline-evil-insert '((t (:background "green" :inherit mode-line)))
  "Powerline evil insert state face." :group 'powerline)

(defface powerline-evil-visual '((t (:background "black" :inherit mode-line)))
  "Powerline evil visual state face." :group 'powerline)

(defface powerline-evil-operator '((t (:background "red" :inherit mode-line)))
  "Powerline evil operator state face." :group 'powerline)

(defface powerline-evil-motion '((t (:background "black" :inherit mode-line)))
  "Powerline evil motion state face." :group 'powerline)

(defface powerline-evil-replace '((t (:background "red" :inherit mode-line)))
  "Powerline evil replace state face." :group 'powerline)

(defface powerline-evil-emacs '((t (:background "yellow" :inherit mode-line)))
  "Powerline evil emacs state face." :group 'powerline)

(defface powerline-evil-unknown '((t (:background "red" :inherit mode-line)))
  "Powerline evil unknown state face." :group 'powerline)



;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-powerline-strip-properties(txt)
  (set-text-properties 0 (length txt) nil txt)
  txt)



;; Powerline fragments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun abdo-powerline-face (face)
  (let* ((active (eq (frame-selected-window) (selected-window)))
         (root (string= (user-login-name) "root")))
    (cond
     ((eq face 'face0) (if active (if root 'powerline-root0 'powerline-active0) 'powerline-inactive0))
     ((eq face 'face1) (if active (if root 'powerline-root1 'powerline-active1) 'powerline-inactive1))
     ((eq face 'face2) (if active (if root 'powerline-root2 'powerline-active2) 'powerline-inactive2))
     ((eq face 'alert) (if active (if root 'powerline-root-alert 'powerline-active-alert) 'powerline-inactive-alert)))))


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
         (face0 (abdo-powerline-face 'face0))
         (separator
          (intern (format "powerline-%s-%s" powerline-default-separator
                          (car powerline-default-separator-dir))))
         (evilstate (abdo-evil-state))
         (narrowstate (if (buffer-narrowed-p) "n" " "))
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
      (propertize (format " %s%s" evilstate narrowstate) 'face evilface)
      (funcall separator evilface face0)))))


(defun abdo-powerline-buffer-name ()
  (let* ((face0 (abdo-powerline-face 'face0))
         (face1 (abdo-powerline-face 'face1))
         (separator
          (intern (format "powerline-%s-%s" powerline-default-separator
                          (car powerline-default-separator-dir)))))
    (powerline-render
     (list
      (powerline-raw "%b " face0 'l)
      (funcall separator face0 face1)))))


(defun abdo-powerline-major-mode ()
  (let* ((face1 (abdo-powerline-face 'face1))
         (face2 (abdo-powerline-face 'face2))
         (majormode (downcase (format-mode-line mode-name))))

    (list
     (propertize " " 'face face1)
     (propertize majormode
           'face face1
           'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"

           'local-map (let ((map (make-sparse-keymap)))
                        (define-key map [mode-line down-mouse-1]
                          `(menu-item ,(purecopy "Menu Bar") ignore
                                      :filter (lambda (_) (mouse-menu-major-mode-map))))
                        (define-key map [mode-line mouse-2]      'describe-mode)
                        (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                        map)))))


(defun abdo-powerline-minor-modes ()
  (let* ((face1 (abdo-powerline-face 'face1))
         (face2 (abdo-powerline-face 'face2))
         (minormodes (delq nil
                      (mapcar
                       (lambda (mm)
                         (when (and mm (not (string-match abdo-modeline-skip-modes-regexp (downcase mm))))
                           (propertize
                            (downcase mm)
                            'face face1
                            'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"

                            'local-map (let ((map (make-sparse-keymap)))
                                         (define-key map [mode-line down-mouse-1]   (powerline-mouse 'minor 'menu mm))
                                         (define-key map [mode-line mouse-2]        (powerline-mouse 'minor 'help mm))
                                         (define-key map [mode-line down-mouse-3]   (powerline-mouse 'minor 'menu mm))
                                         (define-key map [header-line down-mouse-3] (powerline-mouse 'minor 'menu mm))
                                         map))))
                (split-string (format-mode-line minor-mode-alist))))))

     (when (> (safe-length minormodes) 0)
       (list
        (propertize " | " 'face face1)
        (propertize (mapconcat 'identity minormodes " ") 'face face1)))))


(defun abdo-powerline-mode-list ()
  (let* ((face1 (abdo-powerline-face 'face1))
         (face2 (abdo-powerline-face 'face2))
         (separator
          (intern (format "powerline-%s-%s" powerline-default-separator
                          (car powerline-default-separator-dir)))))
    (powerline-render
     (append
      (abdo-powerline-major-mode)
      (when (and (window-valid-p powerline-selected-window)
                 (> (window-total-width powerline-selected-window) 90))
        (abdo-powerline-minor-modes))
      (list
       (propertize " " 'face face1)
       (funcall separator face1 face2))))))


(defun abdo-powerline-middle ()
  (let* ((face2 (abdo-powerline-face 'face2))
         (facealert (abdo-powerline-face 'alert)))
    (powerline-render
     (list
      (when (and (buffer-file-name (current-buffer)) vc-mode)
        (powerline-raw (format-mode-line '(vc-mode vc-mode)) face2 'l))

      (when (> (length abdo-modeline-buffer-alert-list) 0)
        (powerline-raw (format " %s" (abdo-powerline-buffer-alerts)) facealert 'l))

      (when mode-line-process
        (powerline-raw (abdo-powerline-strip-properties (format-mode-line 'mode-line-process)) face2 'l))

      (when global-mode-string
        (powerline-raw (format " %s" (format-mode-line '(global-mode-string global-mode-string))) face2 'r))

      (powerline-fill face2 abdo-powerline-right-width)     ; everything on the right is fixed width
      ;; TODO: truncate this if it gets too long
     ))))


(defun abdo-powerline-position ()
  (let* ((face0 (abdo-powerline-face 'face0))
         (face1 (abdo-powerline-face 'face1))
         (separator
          (intern (format "powerline-%s-%s" powerline-default-separator
                          (cdr powerline-default-separator-dir)))))
    (powerline-render
     (list
      (funcall separator face1 face0)
      (powerline-raw (concat (format-mode-line "%5l %3c  ")
                             (replace-regexp-in-string "%" "%%"
                              (downcase (substring (format-mode-line "%p") 0 3)))
                             "  ")
                             face0 'r)))))


(defun abdo-powerline-state ()
  (let* ((face1 (abdo-powerline-face 'face1))
         (face2 (abdo-powerline-face 'face2))
         (separator
          (intern (format "powerline-%s-%s" powerline-default-separator
                          (cdr powerline-default-separator-dir)))))
    (powerline-render
     (list
      (funcall separator face2 face1)
      (powerline-raw " %*%Z" face1 'r)))))



;; Buffer alerts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-powerline-buffer-alerts ()
  (abdo-modeline-buffer-visit (current-buffer))
  (let ((alert-list abdo-modeline-buffer-alert-list))
    (mapconcat
     (lambda (pair)
       (let ((short (car pair))
             (buf (car (cdr pair))))
         (propertize short
                     'local-map (when (fboundp 'make-mode-line-mouse-map)
                                  (make-mode-line-mouse-map
                                   'mouse-1 `(lambda ()
                                               (interactive "@")
                                               (switch-to-buffer ,buf))))
                     'help-echo (concat "Jump to " (buffer-name buf) " buffer"))))
       alert-list " ")))


(defvar abdo-modeline-buffer-alert-list  '()
  "List of buffers in need of attention")


(defun abdo-modeline-buffer-alert (buffer-or-name &optional short-name)
  (interactive)
  (let* ((buf (get-buffer buffer-or-name))
         (short (or short-name (buffer-name buf))))
    (add-to-list 'abdo-modeline-buffer-alert-list (list short buf))))


(defun abdo-modeline-buffer-visit (buffer-or-name)
  (let ((buf (get-buffer buffer-or-name)))
    (setq abdo-modeline-buffer-alert-list
          (delq nil
                (mapcar (lambda (pair)
                          (if (eq (car (cdr pair)) buf) nil pair))
                        abdo-modeline-buffer-alert-list)))))

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

  ;; Values: arrow-fade, slant, chamfer, alternate, bar, nil, wave, brace,
  ;; roundstub, zigzag, butt, rounded, contour, curve
  (setq powerline-default-separator 'arrow)
  (setq powerline-default-separator-dir '(left . right))

  (setq-default mode-line-format
                (list
                 '(:eval (abdo-powerline-evil-state))       ; evil state
                 '(:eval (abdo-powerline-buffer-name))      ; buffer name
                 '(:eval (abdo-powerline-mode-list))        ; mode list
                 '(:eval (abdo-powerline-middle))
                 '(:eval (abdo-powerline-state))            ; state
                 '(:eval (abdo-powerline-position))         ; position
                 )))



;; Modeline hiding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See http://bzg.fr/emacs-hide-mode-line.html

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; Activate hidden-mode-line-mode
;; (hidden-mode-line-mode 1)

;; If you want to hide the mode-line in all new buffers
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)



;; Tweaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(abdo-modeline-things)
