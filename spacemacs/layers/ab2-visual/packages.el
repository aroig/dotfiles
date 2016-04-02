(setq ab2-visual-packages
      '(
        spaceline
        diminish
        ))

(defun ab2-visual/pre-init-spaceline ()
  (setq
   spaceline-buffer-size-p nil
   spaceline-line-column t
   ))

(defun ab2-visual/post-init-diminish()

  ;; diminish some more modes.
  ;; NOTE: I do not follow the convention that the letter corresponds with the key binding
  (spacemacs|diminish server-buffer-clients "ⓥ" "v")
  (spacemacs|diminish binary-overwrite-mode "Ⓞb" "O")
  (spacemacs|diminish overwrite-mode "Ⓞ" "O")
  (spacemacs|diminish isearch-mode "/" "/")
  )
