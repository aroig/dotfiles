
(defun ab2-mu4e/pre-init-mu4e ()
  ;; paths
  (setq mu4e-mu-home (format "%s/.mu" (getenv "HOME"))
        mu4e-maildir (format "%s/mail" (getenv "HOME"))
        mu4e-attachment-dir  (format "%s/down" (getenv "HOME")))

  ;; folders
  (setq mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder   "/All Mail"
        mu4e-trash-folder  "/Trash")

  ;; config
  (setq mu4e-change-filenames-when-moving nil
        mu4e-get-mail-command nil
        mu4e-compose-signature-auto-include nil
        mail-user-agent 'mu4e-user-agent
        message-kill-buffer-on-exit t
        mu4e-sent-message-behaviour 'sent
        mu4e-use-fancy-chars nil
        )

  ;; headers view
  (setq mu4e-headers-results-limit 500
        mu4e-headers-show-threads t
       ;; mu4e-headers-skip-duplicates t
        mu4e-headers-date-format "%d %b %Y"
        mu4e-headers-time-format "%H:%M"
        mu4e-headers-fields
        '( (:human-date     .  15)
           (:flags          .   5)
           (:from           .  30)
           (:subject        .  nil))
        mu4e-headers-sort-field :date
        mu4e-headers-sort-direction 'descending)

  ;; message view
  (setq mu4e-view-fields
        '(:from :to  :cc :subject :mailing-list :tags :flags :date :maildir :attachments :signature))

  ;; Filter out addresses from mailing lists for completion
  ;; NONTE it does not work. I may have not given my address to the indexer
  ;; (setq mu4e-compose-complete-only-personal t)

  ;; setup some handy shortcuts
  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"       . ?i)
           ("/Sent Mail"   . ?s)
           ("/Drafts"      . ?d)
           ("/Trash"       . ?t)
           ("/All Mail"    . ?a)))

  ;; Bookmarks
  (setq mu4e-bookmarks
        '(("flag:unread"                                          "New"                 ?n)
          ("flag:flagged"                                         "Flagged"             ?g)
          ("tag:\\\\Inbox AND flag:unread"                        "Inbox"               ?i)
        ; ("tag:\\\\Sent AND date:365d..now"                      "Sent"                ?s)

          ("tag:research AND flag:unread"                         "Research"            ?r)
          ("tag:teaching AND flag:unread"                         "Teaching"            ?t)
          ("tag:upc AND flag:unread"                              "University"          ?u)
          ("tag:list AND flag:unread"                             "Lists"               ?l)

          ("tag:arch AND flag:unread"                             "Arch"                ?a)
          ("tag:sage AND flag:unread"                             "Sage"                ?s)
          ("tag:systemd AND flag:unread"                          "Systemd"             ?y)
          ("tag:org AND flag:unread"                              "Org"                 ?o)
          ("tag:devel AND flag:unread"                            "Development"         ?d)

          ("tag:watchlist AND flag:unread"                        "Watchlist"           ?w)
          ("tag:mathoverflow AND flag:unread"                     "Mathoverflow"        ?v)
          ("tag:maths AND flag:unread"                            "Mathematics"         ?m)
          ("tag:arxiv AND flag:unread"                            "Arxiv"               ?x)

          ("tag:news AND flag:unread"                             "News"                ?e)
          ("tag:blog AND flag:unread"                             "Blogs"               ?b)
          ("tag:fun AND flag:unread"                              "Fun"                 ?f)
          ))

  ;; Commonly used tags for completion
  (setq mu4e-tags-completion-list
        '( "upc" "postdoc" "fme" "ma1" "etseib" "research" "seminar"
           "maths" "geometry" "algebra" "physics" "topology"
           "arch" "devel" "msys2" "github" "bitbucket" "bibrain"
           "newsletter" "list" "bug"
           "friends" "family" "bit"
           "gsoc16"
           ))
  )

(defun ab2-mu4e/post-init-mu4e ()

  (with-eval-after-load "mu4e"
    (setq mu4e-context-policy 'pick-first
          mu4e-compose-context-policy 'ask

          mu4e-contexts
          `( ,(make-mu4e-context
               :name "gmail"
               :match-func (lambda (msg)
                             (let ((email "abdo.roig@gmail.com"))
                               (when msg
                                 (or
                                  (mu4e-message-contact-field-matches msg :to email)
                                  (mu4e-message-contact-field-matches msg :cc email)
                                  (mu4e-message-contact-field-matches msg :from email)
                                  ))))
               :vars '( (user-mail-address . "abdo.roig@gmail.com")
                        (user-full-name . "Abdó Roig-Maranges")
                        (message-sendmail-extra-arguments . ("-a" "gmail"))
                        ;;(mu4e-compose-signature . "")
                        ))

             ,(make-mu4e-context
               :name "upc"
               :match-func (lambda (msg)
                             (let ((email "abdo.roig@upc.edu"))
                               (when msg
                                 (or
                                  (mu4e-message-contact-field-matches msg :to email)
                                  (mu4e-message-contact-field-matches msg :cc email)
                                  (mu4e-message-contact-field-matches msg :from email)
                                  ))))
               :vars '( (user-mail-address . "abdo.roig@upc.edu")
                        (user-full-name . "Abdó Roig-Maranges")
                        (message-sendmail-extra-arguments . ("-a" "upc"))
                        ;;(mu4e-compose-signature . "")
                        ))
             )

          ;; This sets `mu4e-user-mail-address-list' to the concatenation of all
          ;; `user-mail-address' values for all contexts. If you have other mail
          ;; addresses as well, you'll need to add those manually.
          mu4e-user-mail-address-list
          (delq nil
                (mapcar (lambda (context)
                          (when (mu4e-context-vars context)
                            (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
                        mu4e-contexts))))


  ;; Custom actions
  (setq mu4e-action-tags-header "X-Keywords")

  ;; Kill process and timers when killing main buffer.
  ;; NOTE: cannot use mu4e-quit because it attempts to close the buffers!
  (add-hook 'mu4e-main-mode-hook
            (lambda ()
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (when mu4e~update-timer
                            (cancel-timer mu4e~update-timer)
                            (setq mu4e~update-timer nil))
                          (mu4e-clear-caches)
                          (mu4e~proc-kill))
                        nil 'local)
              ))

  (add-hook 'mu4e-headers-mode-hook
            (lambda ()
              (add-to-list 'mu4e-headers-actions '("tRetag message" . mu4e-action-retag-message) t)
              ))

  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (add-to-list 'mu4e-view-actions '("tRetag message" . mu4e-action-retag-message) t)
              (add-to-list 'mu4e-view-actions '("bView in browser" . mu4e-action-view-in-browser) t)
              ))

    ;; smtp mail setting
  (setq send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp")

  ;; Hooks
  (add-hook 'mu4e-compose-mode-hook (lambda () (setq fill-column 80)))

  ;; use shr wrapper from mu4e-contrib
  (setq mu4e-html2text-command 'mu4e-shr2text)

  ;; (setq mu4e-html2text-command 'html2text)
  ;; (setq mu4e-html2text-command "html2text")
  ;; (setq mu4e-html2text-command "html2text -utf8 -width 80")
  ;; (setq mu4e-html2text-command "lynx -dump -stdin -width=100 -display_charset=utf-8")
  ;; (setq mu4e-html2text-command "w3m -dump -cols 100 -T text/html")
  ;; (setq mu4e-html2text-command "elinks -dump -force-html -dump-color-mode 1")

  ;; NOTE: I do a sed to remove explicit 'background-color'.
  ;; elinks can do that with lua hooks, but not in -dump mode
  ;; (setq mu4e-html2text-command (concat "sed '/<.*>/ s/background-color:[^;]*;//gI' | "
  ;;                              "elinks" " -no-connect" " -dump" " -force-html"
  ;;                              (format " -dump-width %d" fill-column)
  ;;                              " -dump-color-mode 1"))


  ;; For some reason, this does not always work
  ; (add-hook 'mu4e-view-mode-hook 'abdo-mu4e-ansi-colorize)

  ;; So I advice mu4e-view-message-text to propertize ansi colors
  ;; (defadvice mu4e-view-message-text (after mu4e-view-message-text-ansi (msg))
  ;;   "Replace ansi codes by propertized text"
  ;;   (setq ad-return-value (ansi-color-apply ad-return-value)))
  ;;
  ;; (ad-activate 'mu4e-view-message-text)

)


(defun ab2-mu4e/post-init-persp-mode ()
  (spacemacs|define-custom-layout
   "@mu4e"
   :binding "m"
   :body
   (progn

     (defun ab2/add-mu4e-buffer-to-persp ()
       (persp-add-buffer (current-buffer) (persp-get-by-name "@mu4e")))

     (add-hook 'mu4e-main-mode-hook #'ab2/add-mu4e-buffer-to-persp)
     (add-hook 'mu4e-view-mode-hook #'ab2/add-mu4e-buffer-to-persp)
     (add-hook 'mu4e-headers-mode-hook #'ab2/add-mu4e-buffer-to-persp)
     (add-hook 'mu4e-compose-mode-hook #'ab2/add-mu4e-buffer-to-persp)
     (call-interactively 'mu4e)
     )))
