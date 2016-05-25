
;; registers a project with rtags if it finds a compilation database at the
;; root.
(defun clang/rtags-add-project (root)
  (let ((compdb (concat (directory-file-name root) "/compile_commands.json")))
    (when (file-exists-p compdb)
      (with-temp-buffer (rtags-call-rc "-J" root)))))
