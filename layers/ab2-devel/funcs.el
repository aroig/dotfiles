
;; Compile buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When compilation ends print a message and close the compilation window if sucessful.
;;
;; NOTE: we store the window configuration previous to compile, and restore it after a
;; successful compilation.
;;
;; TODO: need a way to invalidate the stored configuration. Right now it is only
;; invalidated when a compilation succeeds.

(defvar abdo-compile-window-state nil)


(defun ab2/compilation-finished (buffer msg)
  (if (string-match "^finished" msg)
      (progn
        ;; (bury-buffer buffer)
        ;; (when ab2/compile-window-state
        ;;  (set-window-configuration ab2/compile-window-state))
        ;; (setq ab2/compile-window-state nil)

        (message "Successful ☺"))
    (message "Failed ☹")))



;; registers a project with rtags if it finds a compilation database at the
;; root.
(defun ab2-devel/rtags-add-project (root)
  (let ((compdb (concat (directory-file-name root) "/compile_commands.json")))
    (when (file-exists-p compdb)
      (message (format "Starting rtags with compilation database %s" compdb))
      (with-temp-buffer (rtags-call-rc "-J" root)))))

