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
(setq ac-auto-start nil)      ; Do not automatically auto-complete
(setq ac-use-comphist nil)    ; Do not use completion history
(ac-flyspell-workaround)

; add yasnippet ac source. need the setq-default thing
(setq-default ac-sources (append '(ac-source-yasnippet) ac-sources))

; enable auto-complete for latex-mode
(add-to-list 'ac-modes 'latex-mode)



;; yasnippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/Yasnippet

(setq yas-snippet-dirs `(,(concat abdo-emacs-directory "snippets")))
(yas-global-mode 1)
(setq yas-verbosity 1)
