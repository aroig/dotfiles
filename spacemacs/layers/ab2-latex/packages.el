(setq ab2-latex-packages
      '(
        auctex
        reftex
        compile
        outline
        diminish
        ))

(defun ab2-latex/post-init-compile ()
  (with-eval-after-load 'compile
    ;; Matches for latex compilation
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(latex-warning
                   "^LaTeX Warning: .* on input line \\([[:digit:]]+\\)\\.$" ;; Regular expression
                   ab2/compilation-error-latex-file                          ;; Filename
                   1                                                         ;; Line number
                   nil                                                       ;; Column number
                   1))

    (add-to-list 'compilation-error-regexp-alist-alist
                 '(latex-error
                   "^l\\.\\([[:digit:]]+\\)[[:space:]]"  ;; Regular expression
                   ab2/compilation-error-latex-file      ;; Filename
                   1                                     ;; Line number
                   nil                                   ;; Column number
                   2                                     ;; Type (error)
                   1))                                   ;; Highlight

    (add-to-list 'compilation-error-regexp-alist 'latex-error)
    (add-to-list 'compilation-error-regexp-alist 'latex-warning)))


(defun ab2-latex/post-init-auctex ()

  (setq TeX-auto-save nil
        TeX-auto-local ".auto/"
        TeX-parse-self t

        ;; to support draftools active comments
        LaTeX-syntactic-comments nil
        ;; LaTeX-indent-comment-start-regexp "%!\\|%"  ;; % only affects indentation, not filling

        LaTeX-default-environment "equation"

        ;; do not fontify subscript and superscripts
        font-latex-fontify-script nil
        font-latex-fontify-sectioning 'color
        LaTeX-biblatex-use-Biber t)

  ;; hooks
  (add-hook 'TeX-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil
                    fill-column 90
                    use-file-dialog nil            ;; stop asking to open file when errors happen
                    compilation-read-command nil   ;; compilation: do not ask for command
                    )

              ;; auctex make commands
              (add-to-list 'TeX-command-list '("Make"   "make"    TeX-run-compile nil t))
              (add-to-list 'TeX-command-list '("ReMake" "make -B" TeX-run-compile nil t))
              (setq TeX-command-default "Make")

              ;; auctex view commands
              (add-to-list 'TeX-view-program-list '("Zathura" ab2/zathura-sync-view "zathura"))
              (setq TeX-view-program-selection '((output-pdf "Zathura")))

              ;; fontify \& just like &
              (add-to-list 'font-latex-match-warning-keywords '("\&"))))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              ;; outline headings
              (setq outline-promotion-headings
                    '("\\chapter" "\\section" "\\subsection"
                      "\\subsubsection" "\\paragraph" "\\subparagraph"))

              ;; Disable fill inside some environments
              (add-to-list 'LaTeX-indent-environment-list '("tikzpicture"))
              (add-to-list 'LaTeX-indent-environment-list '("tikzcd"))))

  ;; I could not make it work. I need to disable and reenable the
  ;; prettify-symbols-mode for it to take effect.
  ;; (add-hook 'TeX-mode-hook 'turn-on-prettify-symbols-mode)

  (add-hook 'TeX-mode-hook 'outline-minor-mode)
  (add-hook 'TeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'TeX-mode-hook 'auto-fill-mode))


(defun ab2-latex/post-init-reftex()
  (setq reftex-auto-recenter-toc t
        reftex-toc-shown nil))

(defun ab2-latex/init-outline ()
  (use-package outline)
  (spacemacs|diminish outline-minor-mode "ⓞ" " o"))

(defun ab2-latex/post-init-diminish ()
  (spacemacs|diminish reftex-mode "ⓡ" " r"))
