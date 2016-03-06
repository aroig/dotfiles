
(defun ab2/compilation-finished (buffer msg)
  (if (string-match "^finished" msg)
      (progn
        (bury-buffer buffer)
        (when ab2/compile-window-state
          (set-window-configuration ab2/compile-window-state))
        (setq ab2/compile-window-state nil)

        (message "Successful ☺"))
    (message "Failed ☹")))
