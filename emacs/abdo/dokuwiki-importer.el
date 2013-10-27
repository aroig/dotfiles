(provide 'dokuwiki-importer)


(setq doku-import-base-directory (concat org-directory "dokuwiki/"))


(defun doku-import ()
  (interactive)
  (let ((default-directory doku-import-base-directory))
    (call-interactively 'doku-import-file)
  )
)

(defun doku-import-yank ()
  (interactive)
  (let ((buff nil))
    (setq buff 
      (with-temp-buffer
        (yank) 
        (doku-import-transform)
        (buffer-string)
      )
    )
    (insert buff)
  )
)


(defun doku-import-file (file)
  (interactive "fDokuwiki File: ")
  (let ((buff nil))
    (setq buff 
      (with-temp-buffer
        (insert-file-contents file) 
        (doku-import-transform)
        (buffer-string)
      )
    )
    (insert buff)
  )
)



(defun string-repeat (str n)
  (let ((retval ""))
    (dotimes (i n)
      (setq retval (concat retval str)))
    retval))

(defun line-replace (re repl)
  (beginning-of-line)
  (while (re-search-forward re nil t) (replace-match repl))
)


(defun doku-indent-line (depth)
  (insert (string-repeat "  " depth))
)

(defun doku-format-tags (taglist)
  (replace-regexp-in-string "\s+" ":" (concat " " taglist " "))
)


(defun doku-import-transform ()
  (let ((depth 0)
        (literal nil)
        (headline nil)
        (tags nil))

    ; Replace code blocks
    (goto-char (point-min))
    (while (re-search-forward "<code>\\([[:ascii:][:nonascii:]]*?\\)</code>" nil t)
      (replace-match "\n#+BEGIN_SRC\n\\1\n#+END_SRC\n"))

    ; Replacement line by line.
    (goto-char (point-min))
    (while (not (eq (point) (point-max)))
      (narrow-to-region (line-beginning-position) (line-end-position))

      ; Detects source blocks opening.
      (beginning-of-line)(when (re-search-forward "#\\+BEGIN_SRC"nil t) (setq literal t))

      ; If in a source block, indent things right
      (when literal (beginning-of-line)(doku-indent-line depth))

      ; Detects source block closing.
      (beginning-of-line)(when (re-search-forward "#\\+END_SRC"nil t) (setq literal nil))
     
      (unless literal 
        ; Markup 
        (line-replace "''\\(.*?\\)''"                 "=\\1=")
        (line-replace "\\*\\*\\(.*?\\)\\*\\*"         "*\\1*")
        (line-replace "//\\(.*?\\)//"                 "/\\1/")

        ; Kill ugly ::: ... what was I thinking ?!?
        (line-replace "::::\\(.*?\\)::::"             "\\1")
        (line-replace ":::\\(.*?\\):::"               "\\1")
        (line-replace "::\\(.*?\\)::"                 "\\1")

        ; Detect headline
        (beginning-of-line)
        (when (re-search-forward "\\(==+\\)\s*\\(.*?\\)\s*\\(==+\\)" nil t)
          (setq depth (- 7 (length (match-string 1))))
          (replace-match (concat (string-repeat "*" (- (* 2 depth) 1)) " \\2"))
          (setq headline (point))
        )

        ; Detect list item
        (line-replace "^  \\* \\(.*\\)$" (concat (string-repeat "  " depth) "- \\1"))
        (line-replace "^    \\* \\(.*\\)$" (concat (string-repeat "  " (+ 1 depth)) "- \\1"))

        ; Indent text
        (line-replace "^\\(\w.*\\)$" (concat (string-repeat "  " depth) "\\1"))

        ; Links
        (line-replace "\\[\\[\\(.*?\\)|\\(.*?\\)\\]\\]" "[[\\1][\\2]]")

        ; Catch Tags
        (when (re-search-forward "{{tag>\s*\\(.*?\\)\s*}}" nil t)
          (setq tags (match-string 1))
          (replace-match "")
        )
      )
      (widen)

      ; Write tags on the headline
      (when tags
        (save-excursion
          (goto-char headline)
          ; Usual tags
;          (insert (concat "  " (doku-format-tags tags)))
          ; TOPIC tags (for math-ideas)
          (insert (concat "\n" (string-repeat "  " depth) ":PROPERTIES:\n"
                    (string-repeat "  " depth) ":TOPIC: " tags "\n"
                    (string-repeat "  " depth) ":END:\n"))

        )
      )      
      (forward-line 1)
    )
  )
)
