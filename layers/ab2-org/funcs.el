
(defun ab2/find-files-recursively (regexp directory)
  "Return matching files recursively from DIRECTORY."
  (let* ((filelist '())
         (recfilelist '())
         (case-fold-search t)
         (dirlist (directory-files directory t "^[^.#].*$"))
         file subfile)
    ;; loop over directory listing
    (dolist (file dirlist filelist)
      (cond
       ((and (file-regular-p file)
             (string-match regexp file))
        (add-to-list 'filelist file))

       ((file-directory-p file)
        (setq recfilelist
              (ab2/find-files-recursively regexp (concat file "/")))
        (dolist (subfile recfilelist) (add-to-list 'filelist subfile)))))))


(defun ab2/regexp-filter (regexp lst)
  "Filter a list with a regular expression"
  (delq nil (mapcar (lambda (x) (and (stringp x) (string-match regexp x) x)) lst)))



(defun ab2/org-compact-follow ()
  "Make the view compact, then show the necessary minimum."
  (org-overview)
  (let ((org-show-siblings t)
        (org-show-hierarchy-above t))
    (org-reveal))
  (save-excursion
    (org-back-to-heading t)
    (show-children)))


(defun ab2/org-latex-preview-all()
  (interactive)
  (org-preview-latex-fragment 8))



(defun abdo-org-main-buffer ()
  "Loads org main buffer into current window"
  (interactive)
  (find-file abdo-org-main-file)
)


(defun abdo-org-notes-buffer ()
  "Loads org notes buffer into current window"
  (interactive)

  ;; hide modeline
  (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

  ;; open notes file
  (find-file abdo-org-notes-file)
)


(defun ab2/org-generate-latex (filename)
  "Generate latex previews for given file"
  (interactive)
  (with-current-buffer (find-file-noselect filename)
    (let ((current-prefix-arg '(16)))
      (call-interactively 'org-preview-latex-fragment))
    (kill-buffer (current-buffer)))
  nil)


(defun ab2/org-update-agenda()
  "Updates the file containing the list of org files for the
   agenda mode and the id-locations file."
  (interactive)

  ;; Update symlinks to projects and papers
  (compile (format "make -C %s/../Makefile update" org-directory)))



(defun ab2/org-repair-property-drawers ()
  "Fix properties drawers in current buffer.
 Ignore non Org buffers."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((case-fold-search t)
           (inline-re (and (featurep 'org-inlinetask)
                           (concat (org-inlinetask-outline-regexp)
                                   "END[ \t]*$"))))
       (org-map-entries
        (lambda ()
          (unless (and inline-re (org-looking-at-p inline-re))
            (save-excursion
              (let ((end (save-excursion (outline-next-heading) (point))))
                (forward-line)
                (when (org-looking-at-p org-planning-line-re) (forward-line))
                (when (and (< (point) end)
                           (not (org-looking-at-p org-property-drawer-re))
                           (save-excursion
                             (and (re-search-forward org-property-drawer-re end t)
                                  (eq (org-element-type
                                       (save-match-data (org-element-at-point)))
                                      'drawer))))
                  (insert (delete-and-extract-region
                           (match-beginning 0)
                           (min (1+ (match-end 0)) end)))
                  (unless (bolp) (insert "\n"))))))))))))




;; Mobile and calendar exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq abdo-org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\
\)>")
  (setq abdo-org-tstr-regexp (concat abdo-org-tst-regexp "--?-?" abdo-org-tst-regexp))


(defun abdo-org-generate-uids-file (buf)
  (with-current-buffer buf
    (let ((pt (point-min)))
      (org-map-entries
       (lambda ()
         (let ((entry (org-element-at-point)))
           (unless (or (< (point) pt) (org-element-property :ID entry))
             (org-id-get-create)
             (forward-line))))))
    (when (buffer-modified-p buf) (save-buffer))))



(defun abdo-org-generate-uids ()
  (interactive)
  (message "Generating ids")
  ;; TODO: I'd like to put ID's only where they are needed!
  (mapcar (lambda (file)
            (message (format "Updating ID's: %s" file))
            (abdo-org-generate-uids-file (find-file-noselect file)))
          org-agenda-files)
  (org-id-update-id-locations))


(defun abdo-org-export-icalendar-agenda (file-list calendar-file)
  (let ((org-agenda-files file-list)
        (org-icalendar-combined-agenda-file (concat org-ical-directory calendar-file)))

    (message (format "Writing calendar events to %s" calendar-file))
    (org-icalendar-combine-agenda-files)))



(defun abdo-org-export-icalendar ()
  (interactive)
  (require 'ox-icalendar)
  (abdo-org-icalendar-export-setup)

  (abdo-org-export-icalendar-agenda abdo-org-perso-files  "personal.ics")
  (abdo-org-export-icalendar-agenda abdo-org-math-files   "maths.ics")
  (abdo-org-export-icalendar-agenda abdo-org-paper-files  "papers.ics")
  (abdo-org-export-icalendar-agenda abdo-org-teach-files  "teaching.ics")
  (abdo-org-export-icalendar-agenda abdo-org-devel-files  "devel.ics")
  (abdo-org-export-icalendar-agenda abdo-org-bbrain-files "bbrain.ics"))



(defun abdo-org-export-mobile()
  (interactive)
  (when (file-exists-p org-mobile-directory)
    (org-mobile-push)))



;; Latex Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-org-latex-preview-all()
  (interactive)
  (org-preview-latex-fragment 8)
)




(defun abdo-org-icalendar-export-setup()
  ;; Setup ical exports

  ;; I only want to export TODO entries with SCHEDULED or DEADLINE set.
  ;; No timestamps, no TODO's without date.

  (setq org-icalendar-store-UID nil)                  ; need uid's, but I generate them myself
  (setq org-icalendar-with-timestamps nil)            ; no events from plain timestamps. Seems buggy
  (setq org-icalendar-include-todo nil)               ; Do not make TODO's into VTODO entries.

  (setq org-icalendar-categories
        '(all-tags category todo-state))              ; data to set categrory from

  (setq org-icalendar-use-deadline                    ; where to use deadlines
        '(event-if-not-todo event-if-todo todo-due))

  (setq org-icalendar-use-scheduled                   ; where to use scheduled
        '(event-if-not-todo event-if-todo todo-start))
)






;; Archiving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-org-archive-done-buffer (buf)
  (with-current-buffer buf
    (org-map-entries 'org-archive-subtree "/DONE" 'file)
    (when (buffer-modified-p buf) (save-buffer))))


;; Archives done tasks in the current file
(defun abdo-org-archive-done-file ()
  (interactive)
  (abdo-org-archive-done-buffer (current-buffer)))


(defun abdo-org-archive-done-tree ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))


;; Archives done tasks in the subtree
(defun abdo-org-archive-done-all-files ()
  (mapcar (lambda (file)
            (message (format "Archiving done tasks in %s" file))
            (abdo-org-archive-done-buffer (find-file-noselect file)))
          org-agenda-files))



;; Capture Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-org-prompt-file ()
  (let ((file (read-file-name "New file: ")))
    (if (file-exists-p file)
      (message "The file already exists")
      (if (string-match ".*\\.org" file)
        (progn
          (set-buffer (org-capture-target-buffer file))
	  (widen)
        )
        (message "File must be an org file (*.org)")
      )
    )
  )
)


;; Hyperlinks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Open an org-link from a URI (with position).
(defun abdo-org-open-link (fullpath)
  "Opens an URI to a file maybe with position information"
  (interactive "MPath: ")
  (let ((line nil) (path fullpath) (search nil))
    (if (string-match "::\\([0-9]+\\)\\'" fullpath)
      (setq line (string-to-number (match-string 1 path))
	    path (substring path 0 (match-beginning 0))
      )
      (if (string-match "::\\(.+\\)\\'" path)
        (setq search (match-string 1 path)
              path (substring path 0 (match-beginning 0))
        )
      )
    )
    (if (string-match "[*?{]" (file-name-nondirectory path))
      (dired path)
      (org-open-file path 'emacs line search)
    )
  )
)



;; Need link types for org, proj, paper.
(defun abdo-org-hyperlinks-setup ()
  ;; Link abbreviations
  (setq org-link-abbrev-alist
    `(("google"    . "http://www.google.com/search?q=")
      ("gmap"      . "http://maps.google.com/maps?q=%s")
      ("wikipedia" . "http://en.wikipedia.org/wiki/%s")
      ("arxiv"     . "http://arxiv.org/abs/%s")
    )
  )

  ;; Links to files in my org-tree of the form [[org:math/journal]]
  (org-add-link-type "org" 'abdo-org-org-open)
;;  (add-hook 'org-store-link-functions 'abdo-org-org-store-link)

  (org-add-link-type "tex" 'abdo-org-tex-open)
  (org-add-link-type "atag" 'abdo-org-atag-open)
  (org-add-link-type "cali" 'abdo-org-calibre-open)
)


(defun abdo-org-org-open (path)
  "Opens org files relative to org base directory"
  (when (string-match "^\\(.*?\\)\\(::.*\\)?$" (concat path "\n"))
    (let ((link (match-string 1 path))
          (pos (match-string 2 path)))
          (abdo-org-open-link (concat org-directory link ".org" pos))
    )
  )
)


;; TODO: Here I should take into account the current headline and make an appropriate link
;;       This is tricky !

(defun abdo-org-org-store-link ()
  (when (string-match (concat (abdo-escape-regexp org-directory) "\\(.*\\)\\.org") (buffer-file-name))
    (let ((link (match-string 1))
          (description (format "Page %s in org tree" link)))
      (org-store-link-props
        :type "org"
        :link link
        :description description
      )
    )
  )
)

(defun abdo-org-tex-open (path)
  "Opens tex files with a label as position"
  (when (string-match "^\\(.*?\\)\\(::\\(.*\\)\\)?$" (concat path "\n"))
    (let ((link (match-string 1 path))
          (pos (match-string 3 path)))
      (if pos
        (abdo-org-open-link (concat link ".tex::\label{" pos "}"))
        (abdo-org-open-link (concat link ".tex"))
      )
    )
  )
)



(defun abdo-org-calibre-open (path)
  (when (string-match "^\\(.*?\\)\\(::\\(.*\\)\\)?$" (concat path "\n"))
    (let ((query (match-string 1 path))
          (pos (match-string 3 path)))
      (if pos
;        (shell-command "evince &")
        (call-process "cali" nil 0 nil "view" query "-p" pos)
        (call-process "cali" nil 0 nil "view" query)
      )
    )
  )
)


(defun abdo-org-atag-open (query)
  "Opens an agenda buffer with a given tags view"
  (org-tags-view nil query)
)


