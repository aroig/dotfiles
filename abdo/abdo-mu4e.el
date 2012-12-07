(provide 'abdo-mu4e)

;; minor modes
;; -----------------------------------------------
;; TODO: can I make this work nicely, like with notmuch?
;; maybe better!

(define-minor-mode email-mode
       "Email mode. When email mode is enabled, mu4e functions as email client."
      nil                                   ;; The initial value.
      " Email"                              ;; The indicator for the mode line.
      :group 'abdo)


(define-minor-mode news-mode
       "Email mode. When news mode is enabled, mu4e functions as rss reader."
      nil                                   ;; The initial value.
      " News"                               ;; The indicator for the mode line.
      :group 'abdo)

;; Hooks
(add-hook 'news-mode-hook 'abdo-mu4e-news-things)
(add-hook 'email-mode-hook 'abdo-mu4e-mail-things)

;; settings
;; -----------------------------------------------

(defun abdo-mu4e-things ()

  ;; Fancy chars
  ; (setq mu4e-use-fancy-chars t)

  ;; convert html messages to markdown syntax
  (setq mu4e-html2text-command "html2text_py | grep -v '&nbsp_place_holder;'")

  ;; Custom actions
  (setq mu4e-action-tags-header "X-Keywords")
  (add-to-list 'mu4e-headers-actions '("tRetag message" . mu4e-action-retag-message) t)
  (add-to-list 'mu4e-view-actions '("tRetag message" . mu4e-action-retag-message) t)
)


(defun abdo-mu4e-mail-things ()
  (abdo-mu4e-things)

  ;; Paths
  (setq mu4e-maildir "~/Documents/mail")
  (setq mu4e-mu-home "~/.mu-mail")
  (setq mu4e-attachment-dir  "~/Downloads")

  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/All Mail")
  (setq mu4e-trash-folder  "/Trash")

  ;; profile to use for mutag commands
  (setq abdo-mu4e-mutag-profile "mail")

  (setq mu4e-get-mail-command "offlineimap")

  ;; to detect my mail
  (setq mu4e-user-mail-address-list '("abdo.roig@gmail.com" "abdo.roig@upc.edu"))

  ;; overriden in abdo-mu4e-feed-smtp
  (setq user-mail-address "abdo.roig@gmail.com")
  (setq user-full-name  "Abdó Roig-Maranges")


  ;; include in message with C-c C-w
;  (setq message-signature
;        "Abdó Roig-Maranges\nhttp://www.ma1.upc.edu/~abdo\n")

  ;; Copy messages to sent folder. I'll remove the fcc for gmail on abdo-mu4e-feed-msmtp
  (setq mu4e-sent-messages-behavior 'sent)

  ;; identities using gnus alias
  ; TODO: gnus-alias does not work well with mu4e-compose mode
;  (require 'gnus-alias)
;  (gnus-alias-init)
;  (add-hook 'message-setup-hook 'gnus-alias-determine-identity)

  (setq gnus-alias-identity-alist
        '(("gmail"
           nil
           "Abdó Roig-Maranges <abdo.roig@gmail.com>"
           nil ;; No organization header
           nil ;; No extra headers
           nil ;; No extra body text
           nil ;; No signature
           )
          ("upc"
           nil
           "Abdó Roig-Maranges <abdo.roig@upc.edu>"
           nil ;; No organization header
           nil ;; No extra headers
           nil ;; No extra body text
           nil ;; No signature
           )))

  (setq gnus-alias-default-identity "gmail")

  (setq gnus-alias-identity-rules
        '(("gmail" ("to" "abdo.roig@gmail.com" both) "upc")
          ("upc" ("to" "abdo.roig@upc.edu" both) "upc")))

  ;; Set mu4e as default emacs email program
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Filter out addresses from mailing lists for completion
  ;; Note: it does not work. I may have not given my address to the indexer
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
       '( ("flag:unread AND NOT flag:trashed" "Unread messages"      ?u)
          ("date:today..now"                  "Today's messages"     ?t)
          ("date:7d..now"                     "Last 7 days"          ?w)
          ("tag:\\\\Inbox"                    "Gmail Inbox"          ?i)
          ("tag:research"                     "Research"             ?r)
          ("tag:teaching"                     "Teaching"             ?g)
          ("tag:upc"                          "University"           ?n)
          ("tag:devel"                        "Development"          ?d)
          ("mime:image/*"                     "Messages with images" ?p)))

  ;; Times and dates
  (setq mu4e-headers-date-format "%d %b %Y")
  (setq mu4e-headers-time-format "%H:%M")

  ;; show threds
  (setq mu4e-headers-show-threads t)

  ;; fields in list view
  (setq mu4e-headers-fields
        '( (:human-date    .  15)
           (:flags         .   5)
           (:from          .  30)
           (:subject       .  nil)))


  ;; Fields on message view
  (setq mu4e-view-fields
        '(:from :to  :cc :subject :tags :flags :date :maildir :attachments :signature))

  ;; smtp mail setting
  (setq send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Hooks
  (add-hook 'message-send-hook 'abdo-mu4e-feed-msmtp)

  ;; Launch mu4e
  (mu4e)
)


(defun abdo-mu4e-news-things ()
  (abdo-mu4e-things)

  ;; Paths
  (setq mu4e-maildir nil)
  ; (setq mu4e-mu-home "~/.mu-news")

  (setq mu4e-get-mail-command "offlinegreader")

  ;; profile to use for mutag commands
  (setq abdo-mu4e-mutag-profile "news")


  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ; disable send-mail function
  (setq send-mail-function '(lambda (arg)))
)

;; Custom actions
;; -----------------------------------------------

(defun abdo-mu4e (mode)
  (interactive)
  (cond
   ((string= mode "mail") (abdo-mu4e-mail-things))
   ((string= mode "news") (abdo-mu4e-news-things))
   (t (return)))

  ;; Launch mu4e
  (mu4e)
)

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
                 (setq mail-host-address "gmail.com")
                 (setq user-mail-address "abdo.roig@gmail.com")
                 (setq user-full-name "Abdó Roig-Maranges"))

                ((string-match "abdo.roig@upc.edu" from)
                 (message "msmtp account: upc")
                 (setq message-sendmail-extra-arguments '("-a" "upc"))
                 (setq mail-host-address "upc.edu")
                 (setq user-mail-address "abdo.roig@upc.edu")
                 (setq user-full-name "Abdó Roig-Maranges"))

                (t
                 (error (format "Can't recognise address in from field: %s" from))))))))
