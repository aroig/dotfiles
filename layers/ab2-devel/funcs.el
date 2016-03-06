
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
        (bury-buffer buffer)
        (when ab2/compile-window-state
          (set-window-configuration ab2/compile-window-state))
        (setq ab2/compile-window-state nil)

        (message "Successful ☺"))
    (message "Failed ☹")))



(defun colorize-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))


(defun abdo-compile-buffer-things()
  ;; When compilation  finishes
  (add-to-list 'compilation-finish-functions 'abdo-compilation-finished)

  ;; display ansi colors in compilation buffers
  ; NOTE: disabled because it does strange things
  ; (add-hook 'compilation-filter-hook 'colorize-buffer)

  ;; Scroll compilation until first error
  (setq compilation-scroll-output 'first-error)

  ;; Save window state before compile
  (advice-add 'compile :before
              '(lambda (&rest args)
                 (unless abdo-compile-window-state
                   (setq abdo-compile-window-state (current-window-configuration))))))


(defun find-file-upwards (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
  manage to find it, return the containing directory. Else if we
  get to the toplevel directory and still can't find it, return
  nil. Start at startdir or . if startdir not given"

  (let ((dirname (expand-file-name (if startdir startdir ".")))
        (found nil) ; found is set as a flag to leave loop if we find it
        (top nil))  ; top is set when we get to /

    ; While we've neither been at the top last time nor have we found the file.
    (while (not (or found top))
      ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
          (setq top t))

      ; Check for the file and move up one directory if not found
      (if (file-exists-p (expand-file-name filename dirname))
          (setq found t)
        (setq dirname (expand-file-name ".." dirname))))

    ; return statement
    (if found dirname nil)))


(defun abdo-devel-compile ()
  (interactive)
  (cond
   ((file-exists-p (concat default-directory "Makefile"))
    (compile "make -k"))
   ((file-exists-p (find-file-upwards "Makefile"))
    (compile (format "make -k -C \"%s\"" (find-file-upwards "Makefile"))))
   (t (message "Can't find a suitable Makefile"))))



