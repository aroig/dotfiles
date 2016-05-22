;; Utility emacs lisp stuff that have no home

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

