;; Parse LaTeX output to determine the source file
;; Source: https://github.com/RomainPicot/emacs.d
(defun ab2/compilation-error-latex-file ()
  "Analyse the LaTeX output to find the source file in which an error was reported."
  (condition-case nil
      (save-excursion
        (save-match-data
          (let ((found  nil)
                (bound  (point))
                beg
                filename)
            (while (not found)
              ;; Search backward for an opening paren
              (search-backward "(" nil)

              ;; Try to find a matching closing paren
              (condition-case nil
                  (save-excursion
                    (goto-char (scan-sexps (point) 1))

                    (when (or (> (point) bound)         ;; Closing paren after the error message
                              (not (looking-back ")"))) ;; Non-matching closing delimiter
                      (setq found t)))

                ;; Unbalanced expression
                ((error)
                 (setq found t))))

            ;; Extract filename
            (setq beg (1+ (point)))
            (re-search-forward "[[:space:]]" nil)
            (setq filename (buffer-substring beg (- (point) 1)))
            (list filename))))

    ;; Unexpected error
    ((error)
     nil)))


;; Open or close *toc* buffer on the left
;; http://www.emacswiki.org/emacs/AUCTeX
;; http://www.emacswiki.org/emacs/CDLaTeX
(defun ab2/latex-toggle-toc()
  (interactive)
  (if (eq reftex-toc-shown nil)
      (progn
        (setq reftex-toc-split-windows-horizontally t)
        (reftex-toc)
        (setq reftex-toc-split-windows-horizontally nil)
        (setq reftex-toc-shown t)
        )
    (progn
      (setq reftex-toc-shown nil)
      (delete-windows-on "*toc*")
      )
    )
  )


(defun ab2/latex-output-file ()
  (file-truename (concat default-directory (TeX-master-file (TeX-output-extension)))))



;; Synctex interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ab2/zathura-sync-view ()
  (let*
      ((pdf (ab2/latex-output-file))
       (tex (buffer-file-name))
       (line (line-number-at-pos))
       (col (current-column))
       (sock (concat server-socket-dir "/server")))
    (call-process "zathura" nil 0 nil
                  (format "--synctex-forward=%s:%s:%s" line col tex)
                  (when (and (fboundp 'server-running-p) (server-running-p))
                    (format "--synctex-editor-command=emacsclient -s '%s' -e '(ab2/latex-reverse-sync \"%%{input}\" %%{line} %%{column})'" sock))
                  pdf)
    ))


;; reverse sync function
(defun ab2/latex-reverse-sync (tex line col)
  (let
    ((buf (get-buffer (file-name-nondirectory tex)))
     (ret '(:boolean nil)))
    (if (null buf)
        (message "Sorry, %s is not opened..." tex)
      (switch-to-buffer buf)
      (goto-line line)
      (unless (= col -1) (move-to-column col))
      (setq ret '(:boolean t)))
    ret))

