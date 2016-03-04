(setq ab2-office-packages
 '(
   org
   mu4e
   mu4e-contrib
   cl
   compile
   auctex
  ))


(defun ab2-office/pre-init-org ()
  (setq
   ;; directories
   org-directory (format "%s/" (getenv "AB2_WIKI_DIR"))
   abdo-org-attach-subdirectory "data/"
   abdo-org-texmf-directory (concat org-directory "latex/")
   abdo-org-dokuwiki-directory (concat org-directory "dokuwiki/")
   abdo-org-templates-directory (concat org-directory "tpl/")
   org-ical-directory (concat org-directory "ical/")
   org-mobile-directory (concat org-directory "mobile/")
   org-latex-preview-ltxpng-directory (concat org-directory "ltxpng/")
   org-id-locations-file (concat org-directory "etc/id-locations")
   org-directory-wiki (concat org-directory "org/")
   )

  ;; All org files
  (setq org-all-files (abdo-find-files-recursively "\\.org$" org-directory-wiki))
  (setq org-agenda-files '())

  ;; Tech files
  (setq abdo-org-arts-files (abdo-regexp-filter "arts/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-arts-files org-agenda-files))

  ;; Bibrain files
  (setq abdo-org-bbrain-files (abdo-regexp-filter "bbrain/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-bbrain-files org-agenda-files))

  ;; Hardware files
  (setq abdo-org-hard-files (abdo-regexp-filter "hard/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-hard-files org-agenda-files))

  ;; Software files
  (setq abdo-org-soft-files (abdo-regexp-filter "soft/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-soft-files org-agenda-files))

  ;; Devel files files
  (setq abdo-org-devel-files (append abdo-org-hard-files abdo-org-soft-files))

  ;; Math files
  (setq abdo-org-math-files (abdo-regexp-filter "math/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-math-files org-agenda-files))

  ;; Paper files
  (setq abdo-org-paper-files (abdo-regexp-filter "paper/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-paper-files org-agenda-files))

  ;; Personal files
  (setq abdo-org-perso-files (abdo-regexp-filter "perso/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-perso-files org-agenda-files))

  ;; Teaching files
  (setq abdo-org-teach-files (abdo-regexp-filter "teach/.*\\.org$" org-all-files))
  (setq org-agenda-files (append abdo-org-teach-files org-agenda-files))

  ;; Some individual org files
  (setq abdo-org-devel-notes-file (concat org-directory-wiki "soft/notes.org"))
  (setq abdo-org-devel-ideas-file (concat org-directory-wiki "soft/ideas.org"))
  (setq abdo-org-math-notes-file (concat org-directory-wiki "math/notes.org"))
  (setq abdo-org-math-ideas-file (concat org-directory-wiki "math/ideas.org"))
  (setq abdo-org-math-journal-file (concat org-directory-wiki "math/log.org"))
  (setq abdo-org-perso-notes-file (concat org-directory-wiki  "perso/notes.org"))
  (setq abdo-org-perso-journal-file (concat org-directory-wiki "perso/log.org"))
  (setq abdo-org-main-file (concat org-directory-wiki "main.org"))
  (setq abdo-org-notes-file (concat org-directory-wiki "capture.org"))

  ;; default place for notes
  (setq org-default-notes-file (concat org-directory-wiki "capture.org"))

  ;; inbox for mobileorg
  (setq org-mobile-inbox-for-pull (concat org-directory-wiki "mobile.org"))

  ;; Set html output
  (setq org-publish-html-directory (concat org-directory "html/"))




  ;; Some agenda tweaks for speed
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-agenda-ignore-drawer-properties '(effort appt category))
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-inhibit-startup t)

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
          (sequence "FIXME(!)" "|" "FIXED(!)")          ; Issues with the wiki
          (sequence "BUG(!)" "|" "SOLVED(!)")           ; Bugs in software
          (type "WORK" "NOTE" "IDEA" "PROJ" "|")
  ))

  ;; Enforce dependencies
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  ;; Setup latex
  (setq org-export-latex-packages-alist '())


  ;; TODO: latex
  ;; (abdo-org-latex-common-setup)
  ;; (abdo-org-latex-export-setup)
  ;; (abdo-org-latex-preview-setup)

  ;; Setup my export projects
  (setq org-publish-project-alist '())
  (abdo-org-export-projects-setup)

  ;; Setup my capture templates
  (setq org-capture-templates '())
  (abdo-org-capture-templates-setup)




  ;; hooks
  (add-hook 'org-mode-hook
            (lambda ()
              (setq
               indent-tabs-mode nil
               fill-column 100
               org-tags-column (- fill-column)
               org-confirm-elisp-link-function 'y-or-n-p
               )))

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq
               indent-tabs-mode nil
               fill-column 100
               org-agenda-tags-column (- fill-column)
               )))

  (add-hook 'org-agenda-after-show-hook 'abdo-org-compact-follow)

  ;; Just a little darker than background.
  ;; For some reason, I can't set this on the emacs theme
  (custom-set-faces `(org-hide ((t (:foreground "#1f1f1f")))))

  )



(defun ab2-office/post-init-org ()

    ;; Link abbreviations
  (setq org-link-abbrev-alist
    `(("google"    . "http://www.google.com/search?q=")
      ("gmap"      . "http://maps.google.com/maps?q=%s")
      ("wikipedia" . "http://en.wikipedia.org/wiki/%s")
      ("arxiv"     . "http://arxiv.org/abs/%s")
    )
  )

  ;; Links to files in my org-tree of the form [[org:math/journal]]
  ;;  (add-hook 'org-store-link-functions 'abdo-org-org-store-link)

  ;; TODO: fix this
  ;; (org-add-link-type "org" 'abdo-org-org-open)
  ;; (org-add-link-type "tex" 'abdo-org-tex-open)
  ;; (org-add-link-type "atag" 'abdo-org-atag-open)
  ;; (org-add-link-type "cali" 'abdo-org-calibre-open)
)



(defun ab2-office/post-init-mu4e ()

  ;; Custom actions
  (setq mu4e-action-tags-header "X-Keywords")
  ; (add-to-list 'mu4e-headers-actions '("tRetag message" . mu4e-action-retag-message) t)
  ; (add-to-list 'mu4e-view-actions '("tRetag message" . mu4e-action-retag-message) t)
  ; (add-to-list 'mu4e-view-actions '("bView in browser" . mu4e-action-view-in-browser) t)

  ;; For some reason, this does not always work
  ; (add-hook 'mu4e-view-mode-hook 'abdo-mu4e-ansi-colorize)

  ;; So I advice mu4e-view-message-text to propertize ansi colors
  (defadvice mu4e-view-message-text (after mu4e-view-message-text-ansi (msg))
    "Replace ansi codes by propertized text"
    (setq ad-return-value (ansi-color-apply ad-return-value)))

  (ad-activate 'mu4e-view-message-text)
)



(defun ab2-office/post-init-auctex ()
  (add-hook 'LaTeX-mode-hook 'abdo-latex-mode-things)
  (add-hook 'abdo-ispell-hooks 'abdo-latex-ispell-hook)

  )


(defun ab2-office/post-init-compile()

  ;; Matches for the compilation buffer
  (add-to-list 'compilation-error-regexp-alist-alist
               '(latex-warning
                 "^LaTeX Warning: .* on input line \\([[:digit:]]+\\)\\.$" ;; Regular expression
                 ff/compilation-error-latex-file                           ;; Filename
                 1                                                         ;; Line number
                 nil                                                       ;; Column number
                 1))

  (add-to-list 'compilation-error-regexp-alist-alist
               '(latex-error
                 "^l\\.\\([[:digit:]]+\\)[[:space:]]" ;; Regular expression
                 ff/compilation-error-latex-file      ;; Filename
                 1                                    ;; Line number
                 nil                                  ;; Column number
                 2                                    ;; Type (error)
                 1))                                  ;; Highlight

  (setq latex-compilation-error-regexp-alist '(latex-error latex-warning))

  (define-compilation-mode latex-compilation-mode "Latex Compilation"
    "Compilation mode for latex"
    (set (make-local-variable 'compilation-error-regexp-alist)
         latex-compilation-error-regexp-alist)

    (add-to-list 'compilation-finish-functions 'abdo-compilation-finished))



  )
