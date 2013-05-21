(provide 'abdo-devel)


;; General things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-insert-modeline ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n# vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=4:textwidth=80")))



;; Compile buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When compilation ends print a message and close
;; the compilation window if sucessful !

(defun abdo-compilation-finished (buffer msg)
  (if (string-match "^finished" msg)
    (progn
      (bury-buffer "*compilation*")
      (winner-undo)
      (message "Successful :)"))
    (message "Failed :("))
)

(defun abdo-compile-buffer-things()
  ;; When compilation  finishes
  (add-to-list 'compilation-finish-functions 'abdo-compilation-finished)

  ;; Scroll compilation until first error
  (setq compilation-scroll-output 'first-error)
)


;; Prog mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-prog-mode-things()
  (setq-default indent-tabs-mode nil)            ;; No tabs on indent
  (setq tab-width 4)

  ;; Development tools
  (semantic-mode 1)
  (require 'semantic/sb)
;  (ede-minor-mode 1)
)

;; Hook
(add-hook 'prog-mode-hook 'abdo-prog-mode-things)


;; Sage-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sage-command "sage")

;; enable inline output
(add-hook 'sage-startup-after-prompt-hook 'sage-view)


;; You can use commands like
;; (add-hook 'sage-startup-after-prompt-hook 'sage-view-disable-inline-output)
;; (add-hook 'sage-startup-after-prompt-hook 'sage-view-disable-inline-plots)
;; to enable some combination of features


;; Python-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-python-mode-things()
  (setq python-python-command "ipython")
  (setq python-command "ipython")

  ;; Delete trailing whitespaces before save
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

;  (flyspell-prog-mode)                    ;; Enable flyspell on C/C++ comments
;  (abdo-change-dictionary "english")      ;; I always program in english

  (setq python-indent 4)                   ;; indentation
  (abdo-compile-buffer-things)
)

;; Hooks
(add-hook 'python-mode-hook 'abdo-python-mode-things)


;; Lua mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-lua-mode-things()
  (setq lua-indent-level 4)              ;; indentation

  ;; Delete trailing whitespaces before save
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
)

;; Hooks
(add-hook 'lua-mode-hook 'abdo-lua-mode-things)


;; emacs lisp mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-emacs-lisp-mode-things()
  (abdo-compile-buffer-things)

  ;; Delete trailing whitespaces before save
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
)

;; Hooks
(add-hook 'emacs-lisp-mode-hook 'abdo-emacs-lisp-mode-things)




;; C++ mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-c-mode-things()
  (flyspell-prog-mode)                    ;; Enable flyspell on C/C++ comments
  (abdo-change-dictionary "english")      ;; I always program in english

  ;; indentation
  (setq c-basic-offset 4)

  ;; Delete trailing whitespaces before save
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

  (abdo-compile-buffer-things)
)


;; Hooks
(add-hook 'c++-mode-hook 'abdo-c-mode-things)
(add-hook 'c-mode-hook 'abdo-c-mode-things)


;; gdb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gdb-many-windows t)
