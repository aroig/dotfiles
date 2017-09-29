(setq ab2-org-packages
 '(
   org
   persp-mode
   ; calfw
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
        org-log-into-drawer t                ;; Log into LOGBOOK drawer
        org-agenda-start-with-follow-mode t  ;; Enable follow-mode in agenda
        )


  ;; All org files
  (setq org-all-files (ab2/find-files-recursively "\\.org$" org-directory))
  (setq org-agenda-files '())

  ;; Tech files
  (setq abdo-org-arts-files (ab2/regexp-filter "arts/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-arts-files org-agenda-files))

  ;; Bibrain files
  (setq abdo-org-bibrain-files (ab2/regexp-filter "bibrain/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-bibrain-files org-agenda-files))

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

  ;; Work files
  (setq abdo-org-work-files (ab2/regexp-filter "work/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-work-files org-agenda-files))

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
        (bibrain-list   abdo-org-bibrain-files)
        (math-list      abdo-org-math-files)
        (paper-list     abdo-org-paper-files)
        (work-list      abdo-org-work-files)
        (teach-list     abdo-org-teach-files)
        (perso-list     abdo-org-perso-files)

        (ideas-list     (list abdo-org-math-ideas-file))
        (notes-list     (list abdo-org-math-notes-file))

        (mathlog-list   (list abdo-org-math-journal-file))
        (persolog-list  (list abdo-org-perso-journal-file)))

    ;; agenda custom commands
    (setq org-agenda-custom-commands
          (nconc
           ; '(("c" "Calendar" ab2/calfw-calendar ""))

           (ab2/agenda-topic-commands "d" "Development" devel-list)
           (ab2/agenda-topic-commands "b" "Bibrain" bibrain-list)

           (ab2/agenda-topic-commands "h" "Maths" math-list)
           (ab2/agenda-tree-commands "h" "i" "Ideas" 3 ideas-list)
           (ab2/agenda-tree-commands "h" "n" "Notes" 3 notes-list)
           (ab2/agenda-tree-commands "h" "j" "Journal" 4 mathlog-list)

           (ab2/agenda-topic-commands "r" "Papers" paper-list)

           (ab2/agenda-topic-commands "e" "Teaching" teach-list)
           (ab2/agenda-topic-commands "w" "Work" work-list)

           (ab2/agenda-topic-commands "p" "Personal" perso-list)
           (ab2/agenda-tree-commands "p" "j" "Journal" 4 persolog-list))))

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

              ;; add link types
              (org-add-link-type "org"  'ab2/org-org-open)
              (org-add-link-type "tex"  'ab2/org-tex-open)
              (org-add-link-type "atag" 'ab2/org-atag-open)
              ;; (org-add-link-type "cali" 'ab2/org-calibre-open)
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


(defun ab2-org/post-init-org ()
  (spacemacs|diminish orgtbl-mode "Ⓣ" " t"))


(defun ab2-org/post-init-persp-mode ()
  (spacemacs|define-custom-layout
      "@wiki"
    :binding "w"
    :body
    (progn
      (defun ab2/add-org-buffer-to-persp ()
        (let* ((buf (current-buffer))
               (persp (persp-get-by-name "@wiki"))
               (file (buffer-file-name buf))
               (wikidir-re (format "^%s" (regexp-quote org-directory))))
          (when (or (not file) (string-match wikidir-re file))
            (persp-add-buffer buf persp nil))))

      (add-hook 'org-mode-hook #'ab2/add-org-buffer-to-persp)
      (add-hook 'org-agenda-mode-hook #'ab2/add-org-buffer-to-persp)

      (find-file abdo-org-main-file)
      ))

  ;; setup a command line switch for mu4e perspective
  (add-to-list 'command-switch-alist '("wiki" . (lambda (args) (spacemacs/custom-perspective-@wiki))))
  )


;; NOTE: This is half done
(defun ab2-org/init-calfw ()
  ;; Source: https://github.com/syl20bnr/spacemacs/pull/8372

  (setq cfw:org-overwrite-default-keybinding t)

  (use-package calfw
    ; :init
    ; (evil-set-initial-state 'cfw:calendar-mode 'normal)
    )

  (use-package calfw-org)
  )
