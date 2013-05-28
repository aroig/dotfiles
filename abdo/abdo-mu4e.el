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

  ; tweaking visual appearence
  (abdo-mu4e-message-appearence)

  ;; Custom actions
  (setq mu4e-action-tags-header "X-Keywords")
  (add-to-list 'mu4e-headers-actions '("tRetag message" . mu4e-action-retag-message) t)
  (add-to-list 'mu4e-view-actions '("tRetag message" . mu4e-action-retag-message) t)
  (add-to-list 'mu4e-view-actions '("bView in browser" . mu4e-action-view-in-browser) t)
)


(defun abdo-mu4e-mail-things ()
  (abdo-mu4e-things)

  ;; Paths
  (setq mu4e-maildir "~/Documents/mail")
  (setq mu4e-mu-home "~/.mu")
  (setq mu4e-attachment-dir  "~/Downloads")

  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/All Mail")
  (setq mu4e-trash-folder  "/Trash")

  ;; profile to use for mutag commands
  (setq abdo-mu4e-mutag-profile "mail")

  ;; disable get-mail
  (setq mu4e-get-mail-command nil)

  ;; to detect my mail
  (setq mu4e-user-mail-address-list '("abdo.roig@gmail.com" "abdo.roig@upc.edu"))

  ;; Copy messages to sent folder. I'll remove the fcc for gmail on abdo-mu4e-feed-msmtp
  (setq mu4e-sent-messages-behavior 'sent)

  ;; default values
  (setq user-mail-address "abdo.roig@gmail.com")
  (setq user-full-name  "Abdó Roig-Maranges")

  ;; accounts to chose when composing or replying
  (setq abdo-mu4e-account-alist
        '(("gmail"
           (user-mail-address "abdo.roig@gmail.com")
           (user-full-name "Abdó Roig-Maranges"))

          ("upc"
           (user-mail-address "abdo.roig@upc.edu")
           (user-full-name "Abdó Roig-Maranges"))))

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

  ;; sorting field and order
  (setq mu4e~headers-sort-field :date)
  (setq mu4e~headers-sort-direction 'ascending)

  ;; Bookmarks
  (setq mu4e-bookmarks
        '(("flag:unread"                                          "New"                 ?n)
          ("flag:flagged"                                         "Flagged"             ?g)
        ; ("tag:\\\\Inbox AND date:365d..now"                     "Inbox"               ?i)
        ; ("tag:\\\\Sent AND date:365d..now"                      "Sent"                ?s)

          ("tag:research AND flag:unread"                         "Research"            ?r)
          ("tag:teaching AND flag:unread"                         "Teaching"            ?t)
          ("tag:upc AND flag:unread"                              "University"          ?u)
          ("tag:list AND flag:unread"                             "Lists"               ?l)

          ("tag:arch AND flag:unread"                             "Arch"                ?a)
          ("tag:sage AND flag:unread"                             "Sage"                ?s)
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

  ;; Times and dates
  (setq mu4e-headers-date-format "%d %b %Y")
  (setq mu4e-headers-time-format "%H:%M")

  ;; threading and duplicates
  (setq mu4e-headers-results-limit 500)
  (setq mu4e-headers-show-threads t)
  ; (setq mu4e-headers-skip-duplicates t)

  ;; fields in list view
  (setq mu4e-headers-fields
        '( (:human-date    .  15)
           (:flags         .   5)
           (:from          .  30)
           (:subject       .  nil)))

  ;; Fields on message view
  (setq mu4e-view-fields
        '(:from :to  :cc :subject :mailing-list :tags :flags :date :maildir :attachments :signature))

  ;; smtp mail setting
  (setq send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Hooks
  (add-hook 'message-send-hook 'abdo-mu4e-feed-msmtp)
  (add-hook 'mu4e-compose-pre-hook 'abdo-mu4e-set-account)

  (add-hook 'mu4e-compose-mode-hook
        (lambda ()
          (set-fill-column 80)))

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


;; Message view tweaks
;; -----------------------------------------------

(defun abdo-mu4e-message-appearence ()
  ;; Fancy chars
  ; (setq mu4e-use-fancy-chars t)

  ;; convert html messages to markdown syntax
  ; (setq mu4e-html2text-command "html2text")                                             ; python-html2text
  ; (setq mu4e-html2text-command "html2text -utf8 -width 80")                             ; html2text with utf8
  ; (setq mu4e-html2text-command "lynx -dump -stdin -width=100 -display_charset=utf-8")   ; lynx
  ; (setq mu4e-html2text-command "w3m -dump -cols 100 -T text/html")                      ; w3m
  ; (setq mu4e-html2text-command "elinks -dump -force-html -dump-color-mode 1")           ; elinks color

  ; elinks
  (setq mu4e-html2text-command (concat
                                "elinks" " -dump" " -force-html"
                                " -dump-color-mode 1"))
)


;; For some reason, this does not always work
; (add-hook 'mu4e-view-mode-hook 'abdo-mu4e-ansi-colorize)

;; So I advice mu4e-view-message-text to propertize ansi colors
(defadvice mu4e-view-message-text (after mu4e-view-message-text-ansi (msg))
  "Replace ansi codes by propertized text"
  (setq ad-return-value (ansi-color-apply ad-return-value)))

(ad-activate 'mu4e-view-message-text)




(defun mu4e~view-make-urls-clickable ()
  "Turn references at the end into clickable urls that can be
   opened using `mu4e-view-go-to-url'. Monkey patched from
   mu4e-view.el so it only propertizes References section."
  (let ((num 0))
    (save-excursion
      (setq mu4e~view-link-map ;; buffer local
	(make-hash-table :size 32 :weakness nil))
      (re-search-forward "^References$" nil t)
;      (goto-char (point-min))
      (while (re-search-forward mu4e~view-url-regexp nil t)
	(let ((url (match-string 0))
	       (map (make-sparse-keymap)))
	  (define-key map [mouse-1] (mu4e~view-browse-url-func url))
	  (define-key map [?\M-\r] (mu4e~view-browse-url-func url))
      (setq num (+ num 1))
	  (puthash num url mu4e~view-link-map)
	  (add-text-properties 0 (length url)
	    `(face mu4e-view-link-face
	       mouse-face highlight
	       keymap ,map
	       help-echo
	       ("[mouse-1] or [M-RET] to open the link")) url)
	  (replace-match (propertize url 'face 'mu4e-view-url-number-face)))))))
