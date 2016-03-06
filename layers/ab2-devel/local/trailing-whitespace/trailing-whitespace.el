
(defun trailing-whitespace-mode-delete-whitespaces ()
  (when (and (boundp 'trailing-whitespace-mode) trailing-whitespace-mode)
    (delete-trailing-whitespace)))

(define-minor-mode trailing-whitespace-mode
  "Toggle Trailing Whitespace mode.
   Interactively with no argument, this command toggles the mode.
   A positive prefix argument enables the mode, any other prefix
   argument disables it.  From Lisp, argument omitted or nil enables
   the mode, `toggle' toggles the state.

   When Trailing Whitespace mode is enabled, emacs removes all trailing
   whitespaces on save."

  ;; The initial value.
  :init-value nil

  ;; The indicator for the mode line.
  :lighter " TW"

  ;; The minor mode bindings.
  :keymap '()

  :group 'trailing-whitespaces

  ;; the body
  (add-hook 'before-save-hook 'trailing-whitespace-mode-delete-whitespaces nil t))

