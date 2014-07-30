(provide 'abdo-org)

;; Org Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://orgmode.org/

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

(defun abdo-org-global-things()
  ;; NOTE: All paths are absolute!

  ;; WARNING: I'm not sure I'm using attach-subdirectory right.
  ;; NOTE: This is relative!
  (setq abdo-org-attach-subdirectory "data/")

  ;; local tex resources
  (setq abdo-org-texmf-directory (concat org-directory "latex/"))

  ;; dokuwiki old stuff
  (setq abdo-org-dokuwiki-directory (concat org-directory "dokuwiki/"))

  ;; templates
  (setq abdo-org-templates-directory (concat org-directory "tpl/"))

  ;; ical exports
  (setq org-ical-directory
        (concat org-directory "ical/"))

  ;; Local Mobileorg directory.
  ;; From within org I will only sync to this directory.
  (setq org-mobile-directory
        (concat org-directory "mobile/"))

  ;; Where to store latex preview images
  (setq org-latex-preview-ltxpng-directory
        (concat org-directory "ltxpng/"))

  ;; Id locations
  (setq org-id-locations-file
        (concat org-directory "etc/id-locations"))

  ;; Base dir for org files
  (setq org-directory-wiki
        (concat org-directory "org/"))

  ;; Set agenda file list from a file.
  (setq org-agenda-files
        (abdo-find-files-recursively "\\.org$" org-directory-wiki))

  ;; setting topic file lists
  (setq abdo-org-papers-file-list
        (abdo-regexp-filter "papers/.*\\.org$" org-agenda-files))

  (setq abdo-org-proj-file-list
        (abdo-regexp-filter "proj/.*\\.org$" org-agenda-files))

  (setq abdo-org-comp-file-list
        (abdo-regexp-filter "comp/.*\\.org$" org-agenda-files))

  (setq abdo-org-math-file-list
        (abdo-regexp-filter "math/.*\\.org$" org-agenda-files))

  (setq abdo-org-teaching-file-list
        (abdo-regexp-filter "teaching/.*\\.org$" org-agenda-files))

  (setq abdo-org-perso-file-list
        (abdo-regexp-filter "perso/.*\\.org$" org-agenda-files))

  ;; Some individual org files
  (setq abdo-org-devel-notes-file
        (concat org-directory-wiki "comp/notes.org"))

  (setq abdo-org-devel-ideas-file
        (concat org-directory-wiki "comp/ideas.org"))

  (setq abdo-org-math-notes-file
        (concat org-directory-wiki "math/notes.org"))

  (setq abdo-org-math-ideas-file
        (concat org-directory-wiki "math/ideas.org"))

  (setq abdo-org-math-journal-file
        (concat org-directory-wiki "math/log.org"))

  (setq abdo-org-personal-notes-file
        (concat org-directory-wiki  "perso/notes.org"))

  (setq abdo-org-personal-journal-file
        (concat org-directory-wiki "perso/log.org"))

  (setq abdo-org-main-file
        (concat org-directory-wiki "main.org"))

  (setq abdo-org-notes-file
        (concat org-directory-wiki "capture.org"))

  ;; default place for notes
  (setq org-default-notes-file
        (concat org-directory-wiki "capture.org"))

  ;; inbox for mobileorg
  (setq org-mobile-inbox-for-pull
        (concat org-directory-wiki "mobile.org"))

  ;; Set html output
  (setq org-publish-html-directory
        (concat org-directory "html/"))

  ;; Custom agenda commands
  (abdo-org-custom-agenda-setup)

  ;; Interface adjustments
  (abdo-org-interface-tweaks)
)


(defun abdo-org-all-mode-things()
  "Things to do for all org related modes"

  ;; modules to load with org
  ;(add-to-list 'org-modules 'org-id)

  ;; No tabs on indent
  (setq-default indent-tabs-mode nil)

  ;; Want wider columns in org-files
  (setq fill-column 100)

  ;; Activate notifications
  ;; (abdo-org-activate-appt)
)

(defun abdo-org-agenda-things()
  "Things to do on the org agenda mode"

  ;; Do common things
  (abdo-org-all-mode-things)

  ;; Tags column (negative means flush-right)
  (setq org-agenda-tags-column (- fill-column))
)

(defun abdo-org-mode-things()
  "Things to do on the org mode"

  ;; Do common things
  (abdo-org-all-mode-things)

  ;; Tags column (negative means flush-right)
  (setq org-tags-column (- fill-column))

  ;; Confirm elisp code execution with y or n.
  (setq org-confirm-elisp-link-function 'y-or-n-p)

  ;; Latex mode
  ; (turn-on-org-cdlatex)

  ;; File associations
  ;; Desktop dependent stuff!
  (setq org-file-apps '(
      (auto-mode . emacs)
;      ("\\.pdf\\'" . "evince %s")
;      ("\\.pdf::\\([[:digit:]]+\\)\\'" . "evince -p %1 %s")
;      ("\\.x?html\\'" . "chromium %s")
    ))

  ;; Just a little darker than background.
  ;; For some reason, I can't set this on the emacs theme
  (custom-set-faces `(org-hide ((t (:foreground "#1f1f1f")))))
)



(defun abdo-org-load-things()

  ;; Org ID module.
  (setq org-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Org muse tags
;  (require 'org-mtags)

  ;; Encryption of content of headings tagged with :crypt:
  ;(require 'org-crypt)
  ;(org-crypt-use-before-save-magic)
  ;(setq org-tags-exclude-from-inheritance (quote ("crypt")))

  ;; Use symmetric encryption
  ;(setq org-crypt-key nil)

  ;; Numerical priorities. They are character literals!
  (setq org-highest-priority ?1)
  (setq org-lowest-priority ?5)
  (setq org-default-priority ?3)

  ;; Drawers
  (setq org-drawers  '("PROPERTIES" "CLOCK" "LOGBOOK"))

  ;; Setup a logging drawer and make keywords log tiemstamp (!)
  (setq org-log-into-drawer "LOGBOOK")

  (setq org-todo-keywords
    '((sequence "TODO(!)" "|" "DONE(!)")            ; Todos in the external world
      (sequence "WORK(!)" "|")
      (sequence "NOTE(!)" "|")
      (sequence "FIXME(!)" "|" "FIXED(!)")          ; Issues with the wiki
      (sequence "BUG(!)" "|" "SOLVED(!)")           ; Bugs in software
      (sequence "IDEA(!)" "|")
      (sequence "PROJ(!)" "|")
      (sequence "|" "WAIT(!)")                      ; Delay but not forget
  ))

  ;; Enforce dependencies
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  ;; Setup latex
  (setq org-export-latex-packages-alist '())
  (abdo-org-latex-common-setup)
  (abdo-org-latex-export-setup)
  (abdo-org-latex-preview-setup)

  ;; Setup my export projects
  (setq org-publish-project-alist '())
  (abdo-org-export-projects-setup)

  ;; Setup my capture templates
  (setq org-capture-templates '())
  (abdo-org-capture-templates-setup)

  ;; Setup my hyperlinks
  (abdo-org-hyperlinks-setup)
)


;; Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-org-interface-tweaks()
  "Various interface adjustments"

  ;; Startup unfolded
  (setq org-startup-folded t)

  ;; Tweaks
  (setq org-ctrl-k-protect-subtree t)        ;; Prevent killing subtrees
  (setq org-special-ctrl-a/e t)              ;; Adjusts C-a C-e behaviour
  (setq org-odd-levels-only t)               ;; Only odd level stars
  (setq org-hide-leading-stars t)            ;; Hides leading stars
  (setq org-completion-use-ido t)            ;; Use ido for completion

  ;; Agenda
  (setq org-agenda-start-with-follow-mode t) ;; Start agenda in follow mode

  ;; Priority faces
  (defface org-priority-level-1 '((t :inherit default)) "Org priority level 1")
  (defface org-priority-level-2 '((t :inherit default)) "Org priority level 2")
  (defface org-priority-level-3 '((t :inherit default)) "Org priority level 3")
  (defface org-priority-level-4 '((t :inherit default)) "Org priority level 4")
  (defface org-priority-level-5 '((t :inherit default)) "Org priority level 5")

  (setq org-priority-faces `(
    (?1 org-priority-level-1)
    (?2 org-priority-level-2)
    (?3 org-priority-level-3)
    (?4 org-priority-level-4)
    (?5 org-priority-level-5)))
)



;; Latex Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-org-latex-preview-all()
  (interactive)
  (org-preview-latex-fragment 8)
)


(defun abdo-org-latex-common-setup()

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


;; Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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



;; Custom Agenda views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-org-custom-agenda-setup ()

  ;; Some agenda speedups
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-agenda-ignore-drawer-properties '(effort appt category))
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-inhibit-startup nil)

  (let ((devel-list     abdo-org-comp-file-list)
        (research-list  abdo-org-math-file-list)
        (teaching-list  abdo-org-teaching-file-list)
        (perso-list     abdo-org-perso-file-list)
        (papers-list    abdo-org-papers-file-list)

        (ideas-list     (list abdo-org-math-ideas-file))
        (notes-list     (list abdo-org-math-notes-file))

        (mathlog-list   (list abdo-org-math-journal-file))
        (persolog-list  (list abdo-org-personal-journal-file)))
    (setq org-agenda-custom-commands
          `(
            ;; DEVELOPMENT
            ("d" . "Development")

            ; Agenda for development items
            ("da" "Agenda"      agenda ""
             ((org-agenda-files (quote ,devel-list))))

            ; Search in development docs
            ("ds" "Search"      search ""
             ((org-agenda-files (quote ,devel-list))
              (org-agenda-search-view-max-outline-level 2)))

            ; Development TODO list
            ("dt" "Todo"        tags-todo ""
             ((org-agenda-files (quote ,devel-list))))


;            ("dp" "Projects"         tags "+project+LEVEL=1")
;            ("dP" "Projects TODO"    tags-todo "+project")


            ;; RESEARCH
            ("r" . "Research")

            ; Agenda for research items
            ("ra" "Agenda"          agenda ""
             ((org-agenda-files (quote ,(append research-list papers-list)))))

            ; Search in research documents
            ("rs" "Search"          search ""
             ((org-agenda-files (quote ,(append research-list papers-list)))
              (org-agenda-search-view-max-outline-level 2)))

            ; List of research ideas
            ("ri" "Ideas"           tags "+LEVEL=3"
             ((org-agenda-files (quote ,ideas-list))
              (org-agenda-prefix-format "")))

            ; Search in research ideas
            ("rI" "Search ideas"           search ""
             ((org-agenda-files (quote ,ideas-list))
              (org-agenda-search-view-max-outline-level 3)
              (org-agenda-prefix-format "")))

            ; List of research notes
            ("rn" "Notes"           tags "+LEVEL=3"
             ((org-agenda-files (quote ,notes-list))
              (org-agenda-prefix-format "")))

            ; Search in research notes
            ("rN" "Search notes"           search ""
             ((org-agenda-files (quote ,notes-list))
              (org-agenda-search-view-max-outline-level 3)
              (org-agenda-prefix-format "")))

            ; List of papers
            ("rp" "Papers"          tags "+paper+LEVEL=1"
             ((org-agenda-files (quote ,papers-list))
              (org-agenda-search-view-max-outline-level 1)))

            ; Search in research journal
            ("rj" "Search journal"         search ""
             ((org-agenda-files (quote ,mathlog-list))
              (org-agenda-search-view-max-outline-level 4)
              (org-agenda-prefix-format "")))

            ; Research TODO list
            ("rt" "Todo"            tags-todo ""
             ((org-agenda-files (quote ,(append research-list papers-list)))))


            ;; TEACHING
            ("t" . "Teaching")

            ; Agenda for teaching items
            ("ta" "Agenda"          agenda ""
             ((org-agenda-files (quote ,(append teaching-list)))))

            ; Search in teaching documents
            ("ts" "Search"          search ""
             ((org-agenda-files (quote ,(append teaching-list)))
              (org-agenda-search-view-max-outline-level 2)))

            ; Teaching TODO list
            ("tt" "Todo"            tags-todo ""
             ((org-agenda-files (quote ,(append teaching-list)))))


            ;; PERSONAL
            ("p" . "Personal")

            ; Personal agenda
            ("pa" "Agenda"      agenda ""
             ((org-agenda-files (quote ,perso-list))))

            ; Search personal notes
            ("ps" "Search"      search ""
             ((org-agenda-files (quote ,perso-list))
              (org-agenda-search-view-max-outline-level 2)))

            ; Personal TODO list
            ("pt" "Todo"        tags-todo ""
             ((org-agenda-files (quote ,perso-list))))

            ; Search personal journal
            ("pj" "Search journal"     search ""
             ((org-agenda-files (quote ,persolog-list))
              (org-agenda-search-view-max-outline-level 4)
              (org-agenda-prefix-format "")))


            ))))



;; Appt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-org-activate-appt ()
  (setq
   appt-message-warning-time 15 ;; warn 15 min in advance
   appt-display-mode-line t     ;; show in the modeline
   appt-display-format 'window) ;; use our func

  (appt-activate 1)              ;; active appt (appointment notification)
  (display-time)                 ;; time display is required for this...


  ;; this considerably slows down changes in agenda view
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
)



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

  (abdo-org-export-icalendar-agenda abdo-org-perso-file-list "personal.ics")
  (abdo-org-export-icalendar-agenda abdo-org-papers-file-list "papers.ics")
  (abdo-org-export-icalendar-agenda abdo-org-math-file-list "maths.ics")
  (abdo-org-export-icalendar-agenda abdo-org-teaching-file-list "teaching.ics")
  (abdo-org-export-icalendar-agenda abdo-org-comp-file-list "devel.ics"))



(defun abdo-org-export-mobile()
  (interactive)
  (when (file-exists-p org-mobile-directory)
    (org-mobile-push)))



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
      (personal-journal-file abdo-org-personal-journal-file)
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

        ("p" "personal journal" entry (file+datetree ,personal-journal-file)
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



;; Setting the hooks and global stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'org-load-hook 'abdo-org-load-things)   ; No longer works
(add-hook 'org-mode-hook 'abdo-org-mode-things)
(add-hook 'org-agenda-mode-hook 'abdo-org-agenda-things)

(abdo-org-global-things)
(abdo-org-load-things)
