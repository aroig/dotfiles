(provide 'abdo-edit)


;; Undo Tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/UndoTree

;; It seems that evil decides whether to load undo-tree or not ... despite this.
;; Undo-tree does not load for fundamental mode.
(global-undo-tree-mode t)




;; autocomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/AutoComplete

(ac-config-default)
(setq ac-auto-start nil)             ; Do not automatically auto-complete
(ac-flyspell-workaround)

; enable auto-complete for latex-mode
(add-to-list 'ac-modes 'latex-mode)

; (ac-set-trigger-key "TAB")       ; Set trigger



;; yasnippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/Yasnippet

(setq yas-snippet-dirs `(,(concat abdo-emacs-directory "snippets")))
(yas-global-mode 1)



;; Emacs 1on1. Disabled because seems quite unmantained
;; (setq 1on1-minibuffer-frame-flag nil)
;; (require 'oneonone)
;; (1on1-emacs)
