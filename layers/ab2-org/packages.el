(setq ab2-org-packages
 '(
   org
   compile
   persp-mode
  ))


(defun ab2-org/pre-init-org ()
  ;; directories
  (let ((wiki-dir (getenv "AB2_WIKI_DIR")))
    (setq
     org-directory                       (format "%s/org/" wiki-dir)
     org-attach-directory                (format "%s/data/" wiki-dir)
     org-publish-html-directory          (format "%s/html/" wiki-dir)
     org-attach-publish-directory        (format "%s/html/data/" wiki-dir)
     org-ical-directory                  (format "%s/ical/" wiki-dir)
     org-mobile-directory                (format "%s/mobile/" wiki-dir)
     org-latex-preview-ltxpng-directory  (format "%s/ltxpng/" wiki-dir)
     org-id-locations-file               (format "%s/etc/id-locations" wiki-dir)
     ab2/org-templates-directory         (format "%s/templates/" wiki-dir)
     ))

  ;; Org settings
  (setq org-startup-folded t
        org-ctrl-k-protect-subtree t         ;; Prevent killing subtrees
        org-special-ctrl-a/e t               ;; Adjusts C-a C-e behaviour
        org-odd-levels-only t                ;; Only odd level stars
        org-hide-leading-stars t             ;; Hides leading stars
        org-agenda-start-with-follow-mode t)


  ;; All org files
  (setq org-all-files (ab2/find-files-recursively "\\.org$" org-directory))
  (setq org-agenda-files '())

  ;; Tech files
  (setq abdo-org-arts-files (ab2/regexp-filter "arts/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-arts-files org-agenda-files))

  ;; Bibrain files
  (setq abdo-org-bbrain-files (ab2/regexp-filter "bbrain/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-bbrain-files org-agenda-files))

  ;; Hardware files
  (setq abdo-org-hard-files (ab2/regexp-filter "hard/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-hard-files org-agenda-files))

  ;; Software files
  (setq abdo-org-soft-files (ab2/regexp-filter "soft/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-soft-files org-agenda-files))

  ;; Devel files files
  (setq abdo-org-devel-files (append abdo-org-hard-files abdo-org-soft-files))

  ;; Math files
  (setq abdo-org-math-files (ab2/regexp-filter "math/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-math-files org-agenda-files))

  ;; Paper files
  (setq abdo-org-paper-files (ab2/regexp-filter "paper/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-paper-files org-agenda-files))

  ;; Personal files
  (setq abdo-org-perso-files (ab2/regexp-filter "perso/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-perso-files org-agenda-files))

  ;; Teaching files
  (setq abdo-org-teach-files (ab2/regexp-filter "teach/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-teach-files org-agenda-files))

  ;; Some individual org files
  (setq abdo-org-devel-notes-file (concat org-directory "soft/notes.org"))
  (setq abdo-org-devel-ideas-file (concat org-directory "soft/ideas.org"))
  (setq abdo-org-math-notes-file (concat org-directory "math/notes.org"))
  (setq abdo-org-math-ideas-file (concat org-directory "math/ideas.org"))
  (setq abdo-org-math-journal-file (concat org-directory "math/log.org"))
  (setq abdo-org-perso-notes-file (concat org-directory  "perso/notes.org"))
  (setq abdo-org-perso-journal-file (concat org-directory "perso/log.org"))
  (setq abdo-org-main-file (concat org-directory "main.org"))
  (setq abdo-org-notes-file (concat org-directory "capture.org"))

  ;; default place for notes
  (setq org-default-notes-file (concat org-directory "capture.org"))

  ;; inbox for mobileorg
  (setq org-mobile-inbox-for-pull (concat org-directory "mobile.org"))


  ;; Some agenda tweaks for speed
  (setq org-agenda-use-tag-inheritance nil
        org-agenda-ignore-drawer-properties '(effort appt category)
        org-agenda-dim-blocked-tasks nil
        org-agenda-inhibit-startup t)

  (let ((devel-list     abdo-org-devel-files)
        (bbrain-list    abdo-org-bbrain-files)
        (math-list      abdo-org-math-files)
        (paper-list     abdo-org-paper-files)
        (teach-list     abdo-org-teach-files)
        (perso-list     abdo-org-perso-files)

        (ideas-list     (list abdo-org-math-ideas-file))
        (notes-list     (list abdo-org-math-notes-file))

        (mathlog-list   (list abdo-org-math-journal-file))
        (persolog-list  (list abdo-org-perso-journal-file)))

    ;; agenda custom commands
    (setq org-agenda-custom-commands
          `(
            ;; DEVELOPMENT
            ("d" . "Development")

            ("da" "Agenda"      agenda ""
             ((org-agenda-files (quote ,devel-list))))

            ("ds" "Search"      search ""
             ((org-agenda-files (quote ,devel-list))
              (org-agenda-search-view-max-outline-level 2)))

            ("dt" "Todo"        todo ""
             ((org-agenda-files (quote ,devel-list))))

            ("dg" "Tag"         tags-todo ""
             ((org-agenda-files (quote ,devel-list))))


            ;; BIBRAIN
            ("b" . "Bibrain")

            ("ba" "Agenda"      agenda ""
             ((org-agenda-files (quote ,bbrain-list))))

            ("bs" "Search"      search ""
             ((org-agenda-files (quote ,bbrain-list))
              (org-agenda-search-view-max-outline-level 2)))

            ("bt" "Todo"        todo ""
             ((org-agenda-files (quote ,bbrain-list))))

            ("bg" "Tag"         tags-todo ""
             ((org-agenda-files (quote ,bbrain-list))))


            ;; MATHS
            ("h" . "Maths")

            ("ha" "Agenda"         agenda ""
             ((org-agenda-files (quote ,math-list))))

            ("hs" "Search"         search ""
             ((org-agenda-files (quote ,math-list))
              (org-agenda-search-view-max-outline-level 2)))

            ("ht" "Todo"           todo ""
             ((org-agenda-files (quote ,math-list))))

            ("hg" "Tag"            tags-todo ""
             ((org-agenda-files (quote ,math-list))))

            ("hi" "Ideas"          tags "+LEVEL=3"
             ((org-agenda-files (quote ,ideas-list))
              (org-agenda-prefix-format "")))

            ("hI" "Search ideas"   search ""
             ((org-agenda-files (quote ,ideas-list))
              (org-agenda-search-view-max-outline-level 3)
              (org-agenda-prefix-format "")))

            ("hn" "Notes"          tags "+LEVEL=3"
             ((org-agenda-files (quote ,notes-list))
              (org-agenda-prefix-format "")))

            ("hN" "Search notes"   search ""
             ((org-agenda-files (quote ,notes-list))
              (org-agenda-search-view-max-outline-level 3)
              (org-agenda-prefix-format "")))

            ("hj" "Journal"        tags "+LEVEL=4"
             ((org-agenda-files (quote ,mathlog-list))
              (org-agenda-prefix-format "")))

            ("hJ" "Search journal" search ""
             ((org-agenda-files (quote ,mathlog-list))
              (org-agenda-search-view-max-outline-level 4)
              (org-agenda-prefix-format "")))


            ;; PAPERS
            ("r" . "Papers")

            ("ra" "Agenda"      agenda ""
             ((org-agenda-files (quote ,paper-list))))

            ("rs" "Search"      search ""
             ((org-agenda-files (quote ,paper-list))
              (org-agenda-search-view-max-outline-level 2)))

            ("rt" "Todo"        tags-todo ""
             ((org-agenda-files (quote ,paper-list))))

            ("rg" "Tag"         tags ""
             ((org-agenda-files (quote ,paper-list))))


            ;; TEACHING
            ("c" . "Teaching")

            ("ca" "Agenda"          agenda ""
             ((org-agenda-files (quote ,(append teach-list)))))

            ("cs" "Search"          search ""
             ((org-agenda-files (quote ,(append teach-list)))
              (org-agenda-search-view-max-outline-level 2)))

            ("ct" "Todo"            tags-todo ""
             ((org-agenda-files (quote ,(append teach-list)))))

            ("ct" "Tag"             tags ""
             ((org-agenda-files (quote ,(append teach-list)))))


            ;; PERSONAL
            ("p" . "Personal")

            ("pa" "Agenda"         agenda ""
             ((org-agenda-files (quote ,perso-list))))

            ("ps" "Search"         search ""
             ((org-agenda-files (quote ,perso-list))
              (org-agenda-search-view-max-outline-level 2)))

            ("pt" "Todo"           tags-todo ""
             ((org-agenda-files (quote ,perso-list))))

            ("pt" "Tag"            tags ""
             ((org-agenda-files (quote ,perso-list))))

            ("pj" "Journal"        tags "+LEVEL=4"
             ((org-agenda-files (quote ,persolog-list))
              (org-agenda-prefix-format "")))

            ("pJ" "Search journal" search ""
             ((org-agenda-files (quote ,persolog-list))
              (org-agenda-search-view-max-outline-level 4)
              (org-agenda-prefix-format "")))

            )))

  ;; Priority faces
  (defface org-priority-level-1 '((t :inherit default)) "Org priority level 1")
  (defface org-priority-level-2 '((t :inherit default)) "Org priority level 2")
  (defface org-priority-level-3 '((t :inherit default)) "Org priority level 3")
  (defface org-priority-level-4 '((t :inherit default)) "Org priority level 4")
  (defface org-priority-level-5 '((t :inherit default)) "Org priority level 5")

  (setq org-priority-faces
        `((?1 org-priority-level-1)
          (?2 org-priority-level-2)
          (?3 org-priority-level-3)
          (?4 org-priority-level-4)
          (?5 org-priority-level-5)))

  ;; Numerical priorities. They are character literals!
  (setq org-highest-priority ?1
        org-lowest-priority ?5
        org-default-priority ?3)

  ;; Org ID module.
  (setq org-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Drawers
  (setq org-drawers  '("PROPERTIES" "CLOCK" "LOGBOOK")
        org-log-into-drawer "LOGBOOK")

  (setq org-todo-keywords
        '((sequence "TODO(!)" "|" "DONE(!)")            ; Todos in the external world
          (sequence "FIXME(!)" "|" "FIXED(!)")          ; Issues with the wiki
          (sequence "BUG(!)" "|" "SOLVED(!)")           ; Bugs in software
          (type "WORK" "NOTE" "IDEA" "PROJ" "|")
  ))

  ;; Enforce dependencies
  (setq org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)

  ;; Setup latex
  (use-package ob-latex)

  (setq org-highlight-latex-and-related '(latex)
        org-src-fontify-natively t)

  (setq org-export-latex-packages-alist '())

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

;;  (setq org-latex-pdf-process
;;        '("pdflatex -interaction nonstopmode -output-directory %o %f"
;;          "pdflatex -interaction nonstopmode -output-directory %o %f"
;;          "pdflatex -interaction nonstopmode -output-directory %o %f")

  ;; NOTE: this is for previews. What about exporting? I may need to run it
  ;; twice there...
  (setq org-latex-pdf-process
        `(,(concat org-directory "../bin/lualatex.sh --output-directory=%o %f")))

  ;; latex preview
  (setq org-latex-create-formula-image-program 'imagemagick)
  ;;  (setq org-latex-create-formula-image-program 'dvipng)

  ;; Setup my export projects
  (setq org-publish-project-alist
        '(("wiki" :components ("wiki-org" "wiki-attach"))

          ("wiki-org"
           :base-directory ,org-directory :publishing-directory ,org-publish-html-directory
           :base-extension "org" :recursive t :publishing-function org-publish-org-to-html)

          ("wiki-attach"
           :base-directory ,org-attach-directory :publishing-directory ,org-attach-publish-directory
           :base-extension "any" :recursive t :publishing-function org-publish-attachment)))

  ;; Setup my capture templates
  (let ((templates-dir ab2/org-templates-directory)
        (captured-file org-default-notes-file)
        (math-ideas-file abdo-org-math-ideas-file)
        (math-journal-file abdo-org-math-journal-file)
        (perso-journal-file abdo-org-perso-journal-file))
    (setq org-capture-templates
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


;;        ("p" "Project" plain (function abdo-org-prompt-file)
;;          (file ,(concat templates-dir "project-file.tpl")))
;;
;;        ("r" "Paper" plain (function abdo-org-prompt-file)
;;         (file ,(concat templates-dir "paper-file.tpl")))
            )))


  ;; Link setup
  (setq org-link-abbrev-alist
        `(("google"    . "http://www.google.com/search?q=")
          ("gmap"      . "http://maps.google.com/maps?q=%s")
          ("wikipedia" . "http://en.wikipedia.org/wiki/%s")
          ("arxiv"     . "http://arxiv.org/abs/%s")
          )
        )

  ;; Links to files in my org-tree of the form [[org:math/journal]]
  (add-hook 'org-store-link-functions 'ab2/org-org-store-link)

  (org-add-link-type "org"  'ab2/org-org-open)
  (org-add-link-type "tex"  'ab2/org-tex-open)
  (org-add-link-type "atag" 'ab2/org-atag-open)
  ;; (org-add-link-type "cali" 'ab2/org-calibre-open)


  ;; hooks
  (add-hook 'org-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil
                    fill-column 100
                    org-tags-column (- fill-column)
                    org-confirm-elisp-link-function 'y-or-n-p
                    )
             ;; tweal latex previews
              (plist-put org-format-latex-options :scale 1.0)
              (plist-put org-format-latex-options :foreground 'auto)
              (plist-put org-format-latex-options :background 'auto)
              ))

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil
                    fill-column 100
                    org-agenda-tags-column (- fill-column)
                    )))

  (add-hook 'org-agenda-after-show-hook 'ab2/org-compact-follow)

  ;; Just a little darker than background.
  ;; For some reason, I can't set this on the emacs theme
  (custom-set-faces `(org-hide ((t (:foreground "#1f1f1f")))))
  )


(defun ab2-org/post-init-persp-mode ()
  (spacemacs|define-custom-layout
      "@wiki"
    :binding "w"
    :body
    (progn
      (find-file abdo-org-main-file)
      ))

  ;; setup a command line switch for mu4e perspective
  (add-to-list 'command-switch-alist '("wiki" . (lambda (args) (spacemacs/custom-perspective-@wiki))))
  )


