
(defun abdo-mu4e ()
  (interactive)
  (abdo-mu4e-things)
  (mu4e)
)


;; Custom actions
;; -----------------------------------------------


(defvar mu4e-action-tags-completion-list '()
  "List of tags to show for autocompletion")


(defun mu4e-action-retag-message (msg &optional retag-arg)
  "Change tags of a message. Accepts a comma-separated list of
   additions and removals.

   Example: +tag,+long tag,-oldtag

   would add 'tag' and 'long tag', and remove 'oldtag'."
  (let* (
      (path (mu4e-message-field msg :path))
	  (maildir (mu4e-message-field msg :maildir))
	  (oldtags (mu4e-message-field msg :tags))
      (tags-completion (append
                        mu4e-action-tags-completion-list
                        (mapcar (lambda (tag) (format "+%s" tag)) mu4e-action-tags-completion-list)
                        (mapcar (lambda (tag) (format "-%s" tag)) oldtags)))
      (retag (if retag-arg
                 (split-string retag-arg ",")
               (completing-read-multiple "Tags: " tags-completion)))
	  (header  mu4e-action-tags-header)
	  (sep     (cond ((string= header "Keywords") " ")
		     ((string= header "X-Label") " ")
		     ((string= header "X-Keywords") ", ")
		     (t ", ")))
	  (taglist (if oldtags (copy-sequence oldtags) '()))
	  tagstr)
    (dolist (tag retag taglist)
      (cond
	((string-match "^\\+\\(.+\\)" tag)
	  (setq taglist (push (match-string 1 tag) taglist)))
	((string-match "^\\-\\(.+\\)" tag)
	  (setq taglist (delete (match-string 1 tag) taglist)))
	(t
	  (setq taglist (push tag taglist)))))

    (setq taglist (sort (delete-dups taglist) 'string<))
    (setq tagstr (mapconcat 'identity taglist sep))

    (setq tagstr (replace-regexp-in-string "[\\&]" "\\\\\\&" tagstr))
    (setq tagstr (replace-regexp-in-string "[/]"   "\\&" tagstr))

    (if (not (mu4e~contains-line-matching (concat header ":.*") path))
      ;; Add tags header just before the content
      (mu4e~replace-first-line-matching
	"^$" (concat header ": " tagstr "\n") path)

      ;; replaces keywords, restricted to the header
      (mu4e~replace-first-line-matching
	(concat header ":.*")
	(concat header ": " tagstr)
       path))

    (mu4e-message (concat "tagging: " (mapconcat 'identity taglist ", ")))
    (mu4e-refresh-message path maildir)))



(defun abdo-mu4e-retag-message (msg)
  (let ((retag (read-string "Tags: "))
        (path (mu4e-message-field msg :path))
        (maildir (mu4e-message-field msg :maildir)))
    (when retag
      (let ((retaglist (split-string-and-unquote retag)))
        (apply 'call-process "mutag" nil 0 nil "-p" abdo-mu4e-mutag-profile "-t" path "-T" "--" retaglist)
        (message (concat "tagging: " (mapconcat 'identity retaglist ", ")))
        (mu4e~proc-add path maildir)
        ))))

(defun abdo-mu4e-autotag-message (msg)
  (let ((path (mu4e-message-field msg :path))
        (maildir (mu4e-message-field msg :maildir)))
    (apply 'call-process "mutag" nil 0 nil "-p" abdo-mu4e-mutag-profile "-t" path "-A")
    (mu4e~proc-index path mu4e-user-mail-address-list)
    (mu4e~proc-add path maildir)))



;; Functions
;; -----------------------------------------------

(defun abdo-mu4e-set-account ()
  "Set the account for composing a message. If composing new,
   let's the user chose, and otherwise us the to field"
  (let* ((account
          (if nil nil
               ; TODO: get the appropriate account from 'to' and 'cc' fields.
;              mu4e-compose-parent-message
;              (let ((to (mu4e-msg-field mu4e-compose-parent-message :to)))
;                (string-match "/\\(.*?\\)/" maildir)
;                (match-string 1 maildir))

            (ido-completing-read
             "Compose with account: "
             (mapcar #'(lambda (var) (car var)) abdo-mu4e-account-alist)
             nil t nil nil (caar abdo-mu4e-account-alist))))
         (account-vars (cdr (assoc account abdo-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars))))


(defun abdo-mu4e-feed-msmtp ()
 "Choose account label to feed msmtp -a option based on From header in Message buffer;
  This function must be added to message-send-mail-hook for on-the-fly change of From address
  before sending message since message-send-mail-hook is processed right before sending message."
  (interactive)
  (if (message-mail-p)
      (save-excursion
	(let* ((from (save-restriction (message-narrow-to-headers) (message-fetch-field "from"))))

          (cond ((string-match "abdo.roig@gmail.com" from)
                 (message "msmtp account: gmail")
                 (setq message-sendmail-extra-arguments '("-a" "gmail"))
                 (message-remove-header "Fcc")
                 (setq user-mail-address "abdo.roig@gmail.com"))

                ((string-match "abdo.roig@upc.edu" from)
                 (message "msmtp account: upc")
                 (setq message-sendmail-extra-arguments '("-a" "upc"))
                 (setq user-mail-address "abdo.roig@upc.edu"))

                (t
                 (error (format "Can't recognise address in from field: %s" from))))))))



(defun abdo-find-files-recursively (regexp directory)
  "Return .org files recursively from DIRECTORY."
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
              (abdo-find-files-recursively regexp (concat file "/")))
        (dolist (subfile recfilelist) (add-to-list 'filelist subfile)))))))


(defun abdo-regexp-filter (regexp lst)
    (delq nil (mapcar (lambda (x) (and (stringp x)
                                       (string-match regexp x) x)) lst)))



(defun abdo-org-compact-follow ()
  "Make the view compact, then show the necessary minimum."
  (org-overview)
  (let ((org-show-siblings t)
        (org-show-hierarchy-above t))
    (org-reveal))
  (save-excursion
    (org-back-to-heading t)
    (show-children)))


(defun abdo-org-latex-preview-all()
  (interactive)
  (org-preview-latex-fragment 8)
)



(defun abdo-org-home-view ()
  "Sets up a personalized org mode arrangement of windows. This is machine-dependent"
  (interactive)
  (find-file abdo-org-main-file)
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (org-agenda-list)
  (split-window-vertically)
  (other-window -1)
)

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


(defun abdo-org-generate-latex (filename)
  "Generate latex previews for given file"
  (interactive)
  (with-current-buffer (find-file-noselect filename)
    (let ((current-prefix-arg '(16)))
      (call-interactively 'org-preview-latex-fragment))
    (kill-buffer (current-buffer)))
  nil)


(defun abdo-org-update-agenda()
  "Updates the file containing the list of org files for the
   agenda mode and the id-locations file."
  (interactive)

  ;; Update symlinks to projects and papers
  (compile (format "cd %s; make update" org-directory)))


(defun org-repair-property-drawers ()
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




;; Exports Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun abdo-org-export-projects-setup()
  (let (
      (org-attach-directory  (concat org-directory abdo-org-attach-subdirectory))
      (org-attach-publish-directory (concat org-publish-html-directory abdo-org-attach-subdirectory))
    )
    (setq org-publish-project-alist (append org-publish-project-alist
      `(("wiki" :components ("wiki-org" "wiki-attach"))

        ("wiki-org"
        :base-directory ,org-directory :publishing-directory ,org-publish-html-directory
        :base-extension "org" :recursive t :publishing-function org-publish-org-to-html)

        ("wiki-attach"
        :base-directory ,org-attach-directory :publishing-directory ,org-attach-publish-directory
        :base-extension "any" :recursive t :publishing-function org-publish-attachment)
      )
    ))
  )
)


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


(defun abdo-org-latex-common-setup()

  ;; lighlight latex code in org buffers
  (setq org-highlight-latex-and-related '(latex))

  ;; Enable native syntax highlighting for babel
  (require 'ob-latex)
  (setq org-src-fontify-natively t)

  ;; Sets packages on the header
  (setq org-latex-default-packages-alist
        '(
          ; ("AUTO" "inputenc"  t)   ;; not on lualatex
          ; ("T1"   "fontenc"   t)
          ; (""     "fixltx2e"  nil)
          ; (""     "graphicx"  t)
          (""     "longtable" nil)
          (""     "float"     nil)
          (""     "wrapfig"   nil)
          ("normalem" "ulem" t)
          ; (""     "soul"      t)
          (""     "textcomp"  t)
          (""     "marvosym"  t)
          (""     "wasysym"   t)
          ; (""     "latexsym"  t)
          (""     "amssymb"   t)
          (""     "amstext" nil)
          (""     "hyperref"  nil)

          ("" "abdofonts" t)
          ("" "abdoalias" t)
          ("" "comdiag" t)

          "\\tolerance=1000"
          ))

  ;; Latex to pdf process. Two lines means run it two times.
  ;; A last sublist would be the command to do for previews!

;  (setq org-latex-pdf-process
;        '("pdflatex -interaction nonstopmode -output-directory %o %f"
;          "pdflatex -interaction nonstopmode -output-directory %o %f"
;          "pdflatex -interaction nonstopmode -output-directory %o %f")

  ;; NOTE: this is for previews. What about exporting? I may need to run it
  ;; twice there...
  (setq org-latex-pdf-process
        `(,(concat org-directory "bin/lualatex.sh --output-directory=%o %f")))
)


(defun abdo-org-latex-export-setup()
  ;; Nothing for now!
)


(defun abdo-org-latex-preview-setup()
  "Latex preview adjustments"
  (setq org-latex-create-formula-image-program 'imagemagick)
;  (setq org-latex-create-formula-image-program 'dvipng)
  (plist-put org-format-latex-options :scale 1.0)
  (plist-put org-format-latex-options :foreground 'auto)
  (plist-put org-format-latex-options :background 'auto)
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


(defun abdo-org-capture-templates-setup()
  (let (
      (templates-dir abdo-org-templates-directory)
      (captured-file org-default-notes-file)
      (math-ideas-file abdo-org-math-ideas-file)
      (math-journal-file abdo-org-math-journal-file)
      (perso-journal-file abdo-org-perso-journal-file)
    )
    (setq org-capture-templates (append org-capture-templates
      `(("t" "todo" entry (file+headline ,captured-file "Tasks")
          (file ,(concat templates-dir "todo-entry.tpl")))

        ("n" "note" entry (file+headline ,captured-file "Notes")
          (file ,(concat templates-dir "note-entry.tpl")))

        ("i" "idea" entry (file+headline ,captured-file "Ideas")
          (file ,(concat templates-dir "idea-entry.tpl")))

        ("m" "math journal" entry (file+datetree ,math-journal-file)
          (file ,(concat templates-dir "math-journal-entry.tpl")))

        ("p" "personal journal" entry (file+datetree ,perso-journal-file)
          (file ,(concat templates-dir "personal-journal-entry.tpl")))

        ("w" "wiki page" plain (function abdo-org-prompt-file)
          (file ,(concat templates-dir "wiki-file.tpl")))


;        ("p" "Project" plain (function abdo-org-prompt-file)
;          (file ,(concat templates-dir "project-file.tpl")))
;
;        ("r" "Paper" plain (function abdo-org-prompt-file)
;         (file ,(concat templates-dir "paper-file.tpl")))

      )
    ))
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
          (abdo-org-open-link (concat org-directory-wiki link ".org" pos))
    )
  )
)


;; TODO: Here I should take into account the current headline and make an appropriate link
;;       This is tricky !

(defun abdo-org-org-store-link ()
  (when (string-match (concat (abdo-escape-regexp org-directory-wiki) "\\(.*\\)\\.org") (buffer-file-name))
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







;; Latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun abdo-latex-mode-things()

  ;; I need this because I installed auctex myself to get synctex support
;  (load "auctex.el")
;  (load "preview-latex.el")

  (require 'calibre)

  ;; This one is on my emacs-lisp dir.
  (load "cdlatex.el")
  (setq-default indent-tabs-mode nil)            ;; No tabs on indent
  (set-fill-column 90)                           ;; In LaTeX I want it this way

  (outline-minor-mode)                           ;; Outline mode
  (turn-on-reftex)                               ;; RefTex
  ;(turn-on-cdlatex)                             ;; CDLatex
  (setq reftex-auto-recenter-toc t)              ;; Enable reftex auto recentering
  (setq compilation-auto-jump-to-first-error t)  ;; Auto jump on error
  (setq use-file-dialog nil)                     ;; Disable dialog asking for a file on
                                                 ;; errors where the filename field points
                                                 ;; to a missing file.

  (setq TeX-auto-local ".auto/")                 ;; Hide the TeX auto local dir.
  (setq TeX-parse-self t)                        ;; Enable parse on load.
  (setq TeX-auto-save nil)                       ;; Do not generate auto directories.

  (setq reftex-toc-shown nil)                    ;; Disable toc in reftex

  ;; Don't ask for the compilation command
  (setq compilation-read-command nil)

  ;; Set PDF mode
  (TeX-PDF-mode t)

  ;; Set outline mode headings order
  (setq outline-promotion-headings '("\\chapter" "\\section" "\\subsection"
     "\\subsubsection" "\\paragraph" "\\subparagraph"))

  ;; NOTE: Don't need this the way I sync with zathura
  ; (setq TeX-source-correlate-method 'synctex)    ;; Synctex
  ; (setq TeX-source-correlate-mode t)
  ; (setq TeX-source-correlate-start-server t)     ;; Not any more :)

  ;; Setup D-bus interface with evince
  ; (when (featurep 'dbus)
  ;  (abdo-latex-dbus-evince-setup)
  ;  (abdo-latex-dbus-zathura-setup))

  ;; Evince with dbus in viewers
  ; (add-to-list 'TeX-view-program-list '("evince-dbus" abdo-latex-evince-dbus-view))
  ; (add-to-list 'TeX-view-program-selection '(output-pdf "evince-dbus"))

  ;; Enable fixme mode
  ; (add-to-list 'fixme-modes 'latex-mode)
  ; (setq fixme-mode t)

  ;; Adjustments
  (abdo-latex-personal-tweaks)                   ;; My latex adjustments

  ;; Delete trailing whitespaces before save
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

  ;; ac-math sources for auto-complete
  (setq ac-sources
        (append '(ac-source-math-unicode
                  ac-source-math-latex
                  ac-source-latex-commands)
                ac-sources)))



;; Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Latex personal stuff
(defun abdo-latex-personal-tweaks ()
  (interactive)

  ;; enable draftools active comments
  (setq LaTeX-syntactic-comments nil)
  ; (setq LaTeX-indent-comment-start-regexp "%!\\|%")  % only affects indentation, not filling

  ;; label prefix for comdiag
  (add-to-list 'LaTeX-label-alist '("comdiag" . LaTeX-equation-label))

  ;; add environments
  (add-to-list 'LaTeX-environment-list '("comdiag" LaTeX-env-label))
  (add-to-list 'LaTeX-environment-list '("align" LaTeX-env-label))

  ;; default environment
  (setq LaTeX-default-environment "equation")

  ;; do not fontify subscript and superscripts
  ; (setq font-latex-fontify-script nil)

  ;; Keywords
  ;(add-to-list 'font-latex-match-warning-keywords '("\&"))

  ;; Math environments
  (add-to-list 'font-latex-math-environments "comdiag")
  (add-to-list 'font-latex-math-environments "comdiag*")
  (add-to-list 'font-latex-math-environments "align")
  (add-to-list 'font-latex-math-environments "align*")

  ;; Disables fill inside some environments
  (add-to-list 'LaTeX-indent-environment-list '("comdiag"))
  (add-to-list 'LaTeX-indent-environment-list '("comdiag*"))
  (add-to-list 'LaTeX-indent-environment-list '("tikzpicture"))
  (add-to-list 'LaTeX-indent-environment-list '("align*"))
  (add-to-list 'LaTeX-indent-environment-list '("align"))

  ;; Let reftex recognize comdiag as an equation.
  (setq reftex-label-alist '(("comdiag" ?e nil nil t)))
)


(defun abdo-latex-ispell-hook ()
  ;; Disabling spell checking on some parts. Unfortunately, this does not work
  ;; with flyspell, only with ispell mode.

  ;; Disabling spell checking for dollar-equations
  (setq ispell-skip-region-alist
    (append ispell-skip-region-alist
      '(("\\$" . "\\$")
       ("\\$\\$" . "\\$\\$")
      )
    )
  )

  ;; Disabling spell checking for latex keywords and environments
  (setq ispell-tex-skip-alists
    (list
      (append (car ispell-tex-skip-alists)
        '(("\\\\includegraphics" ispell-tex-arg-end)
          ("\\\\bibliography"    ispell-tex-arg-end)
          ("\\\\eqref"           ispell-tex-arg-end)
        )
      )
      (append (cadr ispell-tex-skip-alists)
        '(("equation\\*?" . "\\\\end[ \t\n]*{[ \t\n]*equation\\*?[ \t\n]*}")
          ("align\\*?" . "\\\\end[ \t\n]*{[ \t\n]*align\\*?[ \t\n]*}")
          ("comdiag\\*?" . "\\\\end[ \t\n]*{[ \t\n]*comdiag\\*?[ \t\n]*}")
          ("tikzpicture" . "\\\\end[ \t\n]*{[ \t\n]*tikzpicture[ \t\n]*}")
          ("array" ispell-tex-arg-end 1)
        )
      )
    )
  )
)

;; Some other tweaks I do not use
;;
;; (setq ispell-extra-args '("-t" "--sug-mode=ultra"))

;; That flyspell-babel only changed language for the document. Not for comments !
;; May be useful, but not right now.

;  (require 'flyspell-babel)           ;; Change language according to babel

  ;; Set the correspondence to ispell dictionaries
;  (setq flyspell-babel-to-ispell-alist
;   '(("english" "american")
;     ("catalan" "catalan")
;     ("spanish" "spanish")
;     ("german" "ngerman")))
;
;  (flyspell-babel-setup)


;; Navigating
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Parse LaTeX output to determine the source file
;; Source: https://github.com/RomainPicot/emacs.d
(defun ff/compilation-error-latex-file ()
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


(defun abdo-latex-completing-read (prompt collection default)
  (let* ((fullprompt (if default
                         (format "%s (default %s): " prompt default)
                       (format "%s: " prompt))))
         (ido-completing-read fullprompt collection nil t nil nil default)))


(defun abdo-latex-find-label (label &optional file other-window)
  (let*
      ((buffer (if file (find-file-noselect file) (current-buffer)))
       (regexp (format reftex-find-label-regexp-format (regexp-quote label))))

    (when buffer
      (if other-window
          (switch-to-buffer-other-window buffer)
        (switch-to-buffer buffer))

      (push-mark)
      (goto-char (point-min))
      (re-search-forward regexp nil t)
      (reftex-unhighlight 0))))


(defun abdo-latex-goto-label (label &optional other-window)
  "Prompt for a label (with completion) and jump to the location of this label.
   Optional prefix argument OTHER-WINDOW goes to the label in another window."
  (interactive "P")
  (reftex-access-scan-info t)
  (let* ((docstruct (symbol-value reftex-docstruct-symbol))
         (candidates (delq nil (mapcar (lambda (x) (when (stringp (car x)) (car x))) docstruct)))
         ;; If point is inside a \ref{} or \pageref{}, use that as default value.
         (default (when (looking-back "\\\\\\(?:page\\)?ref{[-a-zA-Z0-9_*.:]*")
                    (reftex-this-word "-a-zA-Z0-9_*.:")))
         (label (abdo-latex-completing-read "Label" candidates default))
         (selection (assoc label docstruct))
         (file   (nth 3 selection)))

    (abdo-latex-find-label label file other-window)))


(defun abdo-latex-insert-ref (label)
  "Prompt for a label (with completion) and insert a referene to it."
  (interactive "P")
  (reftex-access-scan-info t)
  (let* ((docstruct (symbol-value reftex-docstruct-symbol))
         (candidates (delq nil (mapcar (lambda (x) (when (stringp (car x)) (car x))) docstruct)))
         (label (abdo-latex-completing-read "Label" candidates nil)))
    (insert (format "\\ref{%s}" label))))





;; Show compilation buffer
(defun abdo-latex-show-compilation()
  (interactive)
  (switch-to-buffer-other-window "*compilation*")
)


;; Make a diff tex.
(defun abdo-latex-make-diff(revision)
  (interactive "sRevision: ")
  (shell-command (format "texa diff %s" revision))
  (find-file-other-window (format "%s.diff.tex"
    (TeX-master-file))
  )
)

(defun abdo-latex-output ()
  (file-truename (concat default-directory (TeX-master-file (TeX-output-extension)))))


;; Auctex and CDLatex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/AUCTeX
;; http://www.emacswiki.org/emacs/CDLaTeX


;; Open or close *toc* buffer on the left
(defun abdo-latex-toggle-toc()
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


;; Synctex interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-latex-view ()
  (interactive)
  (let
    ((pdf (abdo-latex-output)))
    (abdo-latex-zathura-view pdf)
    ))


(defun abdo-latex-forward-sync ()
  (interactive)
  (let
      ((pdf (abdo-latex-output))
       (tex (buffer-file-name))
       (line (line-number-at-pos))
       (col  (current-column)))
    (message (format "synctex forward: %s %s %s %s" pdf tex line col))
    (abdo-latex-zathura-forward-sync pdf tex line col)
    ))


(defun abdo-latex-reverse-sync (tex line col)
  (interactive)
  (message (format "synctex reverse: %s %s %s" tex line col))
  (abdo-latex-zathura-reverse-sync tex line col))


(defun abdo-latex-dbus-setup ()
  (when (featurep 'dbus) (abdo-latex-dbus-zathura-setup)))



;; Zathura sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; view pdf in zathura
(defun abdo-latex-zathura-view (pdf)
  (call-process "zathura" nil 0 nil pdf))


;; zathura forward sync
(defun abdo-latex-zathura-forward-sync (pdf tex line col)
  (call-process "zathura" nil 0 nil
                (format "--synctex-forward=%s:%s:%s" line col tex) pdf))


;; zathura reverse sync
(defun abdo-latex-zathura-reverse-sync (tex line col)
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


;; Setup D-bus interface for zathura reverse sync
(defun abdo-latex-dbus-zathura-setup()
  (when (and (eq window-system 'x) (fboundp 'dbus-register-signal))
    (message "Registering dbus method for zathura")

    ;; service
    ; (dbus-register-service :session "org.gnu.Emacs")

    ;; Reverse sync method
    (dbus-register-method :session "org.gnu.Emacs" "/synctex"
                          "org.gnu.Emacs" "reverse_sync"
                          'abdo-latex-zathura-reverse-sync)

    ;; Make stuff introspectable
    (dbus-register-method :session "org.gnu.Emacs" "/"
                          dbus-interface-introspectable "Introspect"
                          'abdo-latex-dbus-slash-introspect)

    (dbus-register-method :session "org.gnu.Emacs" "/synctex"
                          dbus-interface-introspectable "Introspect"
                          'abdo-latex-dbus-slash-synctex-introspect)

    ))


;; Introspection methods
(defun abdo-latex-dbus-slash-introspect ()
  "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"
  \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">
  <node name='/'>
    <interface name='org.freedesktop.DBus.Introspectable'>
    <method name='Introspect'>
    <arg name='xml_data' type='s' direction='out'/>
    </method>
    </interface>
    <node name='synctex'>
    </node>
  </node>")


(defun abdo-latex-dbus-slash-synctex-introspect ()
"<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"
  \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">
  <node name='/synctex'>
    <interface name='org.freedesktop.DBus.Introspectable'>
    <method name='Introspect'>
    <arg name='xml_data' type='s' direction='out'/>
    </method>
    </interface>
    <interface name='org.gnu.Emacs.synctex'>
      <method name='reverse_sync'>
        <arg name='tex' direction='in' type='s' />
        <arg name='line' direction='in' type='i' />
        <arg name='col' direction='in' type='i' />
        <arg name='ret' direction='out' type='b' />
      </method>
    </interface>
  </node>")




;; evince sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup D-bus interface with evince
(defun abdo-latex-dbus-evince-setup()
  (when (and (eq window-system 'x) (fboundp 'dbus-register-signal))
    (dbus-register-signal
      :session nil "/org/gnome/evince/Window/0"
      "org.gnome.evince.Window" "SyncSource"
      'abdo-latex-evince-reverse-sync
    )
  )
)


;; Handler for evince reverse search
;; Adapted from: http://www.mail-archive.com/auctex@gnu.org/msg04175.html

;; The optional timestamp is to mantain backwards compatibility.
;; Once all my systems run ubuntu 11.10, will be able to remove the &optional.
(defun abdo-latex-evince-reverse-sync (file linecol &optional timestamp)
  (let
    ((buf (get-buffer (file-name-nondirectory file)))
    (line (car linecol))
    (col (cadr linecol)))
    (if (null buf)
      (message "Sorry, %s is not opened..." file)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1) (move-to-column col))
    )
  )
;;  (sleep-for 0.5)                                    ;; Give time to release the click
;;  (select-frame-set-input-focus (selected-frame))    ;; Focus on the frame
)

;; Forward search.
;; Adapted from http://dud.inf.tu-dresden.de/~ben/evince_synctex.tar.gz

(defun abdo-latex-evince-forward-sync (pdffile texfile line)
  (let*
    ((dbus-name
      (dbus-call-method :session
        "org.gnome.evince.Daemon"  ; service
        "/org/gnome/evince/Daemon" ; path
        "org.gnome.evince.Daemon"  ; interface
        "FindDocument"
        (concat "file://" pdffile)
        t                          ; Open a new window if the file is not opened.
      ))
      (time (current-time))
      (high (car time))
      (low (cadr time))
      (timestamp (+ (* high (expt 2 16)) low)))

    (sleep-for 1)                  ; Need to let evince start
    (dbus-call-method :session
      dbus-name
      "/org/gnome/evince/Window/0"
      "org.gnome.evince.Window"
      "SyncView"
      texfile
      (list :struct :int32 line :int32 1)
      timestamp)

    ;; TODO: Try to set focus on evince
  )
)


;; View pdf in evince and sync current line via dbus
(defun abdo-latex-evince-dbus-view ()
  (interactive)
  (let
    ((pdf (file-truename (concat default-directory "out/" (TeX-master-file (TeX-output-extension)))))
    (tex (buffer-file-name))
    (line (line-number-at-pos)))
    (abdo-latex-evince-forward-sync pdf tex line)
  )
)
