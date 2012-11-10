(provide 'abdo-devel)



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
  ;; Delete trailing whitespaces before save
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)

  (setq-default indent-tabs-mode nil)            ;; No tabs on indent

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

;  (flyspell-prog-mode)                    ;; Enable flyspell on C/C++ comments
;  (abdo-change-dictionary "english")      ;; I always program in english

  (setq python-indent 2)                   ;; indentation
  (abdo-compile-buffer-things)
)

;; Hooks
(add-hook 'python-mode-hook 'abdo-python-mode-things)


;; Lua mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-lua-mode-things()
  (setq lua-indent-level 4)              ;; indentation
)

;; Hooks
(add-hook 'lua-mode-hook 'abdo-lua-mode-things)


;; emacs lisp mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-emacs-lisp-mode-things()
  (abdo-compile-buffer-things)
)

;; Hooks
(add-hook 'emacs-lisp-mode-hook 'abdo-emacs-lisp-mode-things)




;; C++ mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-c++-mode-things()
  (flyspell-prog-mode)                    ;; Enable flyspell on C/C++ comments
  (abdo-change-dictionary "english")      ;; I always program in english

  (abdo-compile-buffer-things)
)


;; Hooks
(add-hook 'c++-mode-hook 'abdo-c++-mode-things)
(add-hook 'c-mode-hook 'abdo-c++-mode-things)


;; gdb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gdb-many-windows t)
