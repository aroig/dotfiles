; (require 'abdo-utils)

(defvar calibre-db-bin "calibredb")

(defvar calibre-format-priority '("pdf", "djvu", "epub"))

(defface calibre-item-face
  '((t (:underline nil))) "calibre item face")


(defun calibre-open-by-id (id)
  (interactive "sId: ")

)

(defun calibre-open-by-path (path)
  (call-process "xdg-open" nil 0 nil path)
)

(defun calibre-truncate (string len)
  (if (> (length string) len) (concat (substring string 0 (- len 3)) "...") string))



(defun calibre-process-and-insert-line (buf line)
  (string-match "\\([0-9]+\\)\s*#\\(.*?\\)\s*#\\(.*?\\)\s*#\\[\\(.*\\)\\]" line)
  (let ((inhibit-read-only t)
	(id (match-string 1 line))
	(title (match-string 2 line))
	(authors (match-string 3 line))
	(files (match-string 4 line)))
    (with-current-buffer buf
      (insert (format "%-60s %-30s" (calibre-truncate title 60) (calibre-truncate authors 30)))
      (lexical-let ((path files))
	(make-text-button (line-beginning-position) (line-end-position)
                          'face 'calibre-intem-face
			  'action (lambda (button) (interactive)
			    (calibre-open-by-path path))))
      (insert "\n")
      (setq buffer-read-only t)
      )))


(defun calibre-find (query)
  (interactive "sSearch: ")
  ; Create calibre buffer
  (let ((calibre-buffer (get-buffer-create "*Calibre*")))
    (with-temp-buffer
      (call-process calibre-db-bin nil (current-buffer) nil "list"
		    "--line-width=10000" "--separator=#"
		    "--fields=title,authors,formats" "-s" query)

      ; TODO: put titles in calibre bufer?
      (with-current-buffer calibre-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
	  (insert (format "Query: %s\n\n" query))))

      (goto-char (point-min))
      (while (search-forward-regexp ".*\n\\|.+" nil t)
	(let ((line (match-string 0)))
	  (when (and line (> (length (utils-strip line)) 0))
            (unless (string-match "#title" line)
	      (calibre-process-and-insert-line calibre-buffer line))))
	))
    (pop-to-buffer calibre-buffer)
    (goto-char (point-min))))


(provide 'calibre)
