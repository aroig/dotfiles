
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; global
(defvar abdo-chat-current-status "online")

;; rcirc
(defvar abdo-rcirc-alert-keyword-regexp "\\b\\(abtwo\\|ab2\\|Abdó\\|Abdo\\|abdo\\|abdó\\)\\b")
(defvar abdo-rcirc-alert-ignore-user-regexp "NickServ")
(defvar abdo-rcirc-log-ignore-regexp "abtwo\\|NickServ\\|.*\\.freenode\\.net")

;; jabber
(defvar abdo-jabber-hidden nil)
(defvar abdo-jabber-alert-keyword-regexp "\\b\\(abtwo\\|ab2\\|Abdó\\|Abdo\\|abdo\\|abdó\\)\\b")
(defvar abdo-jabber-alert-user-regexp nil)
(defvar abdo-jabber-alert-ignore-user-regexp nil)

;; twitter
(defvar abdo-twitter-alert-keyword-regexp "\\b\\(abtwo\\|ab2\\|Abdó\\|Abdo\\|abdo\\|abdó\\)\\b")
(defvar abdo-twitter-alert-ignore-user-regexp nil)


;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-chat-status (status)
  (interactive (list (ido-completing-read "Status: " '("online" "away" "dnd"))))

  (let ((oldstatus abdo-chat-current-status))
    (cond
     ((string= status "online")
      (when (not abdo-jabber-hidden) (jabber-send-presence "" "" jabber-default-priority))
      (abdo-rcirc-away "")
      (setq abdo-chat-current-status "online"))

     ((string= status "away")
      (when (not abdo-jabber-hidden) (jabber-send-presence "away" "" jabber-default-priority))
      (abdo-rcirc-away "away")
      (setq abdo-chat-current-status "away"))

     ((string= status "away")
      (when (not abdo-jabber-hidden) (jabber-send-presence "dnd" "" jabber-default-priority))
      (abdo-rcirc-away "do not disturb")
      (setq abdo-chat-current-status "dnd")))
    oldstatus))


(defun abdo-chat-away ()
  (interactive)
  (setq abdo-chat-current-status (abdo-chat-status "away")))


(defun abdo-chat-back ()
  (interactive)
  (abdo-chat-status abdo-chat-current-status))


(defun abdo-chat-connect ()
  (interactive)
  (abdo-open-rcirc)
  (abdo-open-twitter)
  (abdo-open-jabber)
  (switch-to-buffer "*google-talk*"))


(defun abdo-chat-disconnect ()
  (interactive)
  (abdo-close-rcirc)
  (abdo-close-jabber)
  (abdo-close-twitter))


;; Some functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-notify-message (who title msg)
  (start-process "notify" nil "notify-send" "-a" (format "%s" who)
                 (format "%s" title) (format "%s" msg)))


(defun abdo-urgency-hint (frame arg &optional source)
  (let* ((wm-hints (append (x-window-property
			    "WM_HINTS" frame "WM_HINTS" source nil t) nil))
	 (flags (car wm-hints)))
    (setcar wm-hints
	    (if arg
		(logior flags #x00000100)
	      (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))


(defun abdo-chat-notify (serv user nick text buf)
  "Notify a chat event"
  (message (format "%s: notification from %s" serv nick))
  (when
    (abdo-notify-message serv (format "%s: %s" serv nick) text)
    (abdo-urgency-hint (selected-frame) t)
    (when buf (abdo-modeline-buffer-alert buf nick))
    ))


(defun frame-has-focus-p (frame)
  (interactive)

  (when (and (featurep 'x) window-system)
    (let* ((active-window (x-window-property
                           "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t))
           (active-window-id (if (numberp active-window)
                                 active-window
                               (string-to-number
                                (format "%x00%x"
                                        (car active-window)
                                        (cdr active-window)) 16)))
           (emacs-window-id (string-to-number
                             (frame-parameter frame 'outer-window-id))))
      (= emacs-window-id active-window-id))))


(defun abdo-buffer-notify-p (buf)
  (or (not (memq (selected-window) (get-buffer-window-list buf)))       ; buffer not active
      (not (frame-has-focus-p (window-frame (get-buffer-window buf))))  ; frame doesn't have focus
      (not (eq (frame-visible-p (window-frame (selected-window))) t)))) ; frame is not visible




;; twitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: saving oauth token.
;; The first time, run twit end open the verification page and enter the PIN.
;; Then save the value of the variable twittering-oauth-access-token-alist in
;; the corresponding file in ~/priv

(defun abdo-twitter-global-things ()
  (defvar abdo-twittering-mode-oauth-token-file "~/priv/var/twittering-mode/oauth-token")
  (setq twittering-username "abroig")

  (add-hook 'twittering-mode-hook 'abdo-twittering-mode-things)
)


(defun abdo-twittering-mode-things ()
  (setq twittering-initial-timeline-spec-string
        '(":home"
          ":direct_messages"
;          ":replies"
;          ":favorites"
;          ":search/emacs/"
;          "user_name/list_name")
          ))

  ;; update timeline every 5 minutes
  (setq twittering-timer-interval 300)

  ;; TODO: write a notification function
  (add-hook 'twittering-new-tweets-hook 'abdo-twittering-mode-notify)
)

(defun abdo-twittering-mode-notify ()
  ;; loop over new tweets
  (dolist (el twittering-new-tweets-statuses)
    (let ((name (or (cdr (assoc 'user-name el)) ""))
          (user (or (cdr (assoc 'user-screen-name el)) ""))
          (text (or (cdr (assoc 'text el)) "")))

      (cond
       ;; notify mentions
       ((string-match (concat "@" (downcase twittering-username)) (downcase text))
        (abdo-chat-notify "twitter" name user text (get-buffer ":home")))

       ;; notify keyword match
       ((or (string-match abdo-twitter-alert-keyword-regexp text)
            (string-match abdo-twitter-alert-keyword-regexp user))
        (abdo-chat-notify "twitter" name user text (get-buffer ":home")))

       )))
)


(defun abdo-open-twitter ()
  (interactive)
  ;; load oauth tokens
  (setq twittering-oauth-access-token-alist
        (read
         (with-temp-buffer
           (insert-file-contents abdo-twittering-mode-oauth-token-file)
           (buffer-string))))

  (twittering-mode)
)


(defun abdo-close-twitter ()
  (interactive)

)


;; jabber
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-jabber-presence-notify (from oldstatus newstatus statustext proposed-alert)
  (when (and (and abdo-jabber-alert-user-regexp
                  (string-match abdo-jabber-alert-user-regexp (jabber-jid-displayname from)))
             (eq oldstatus nil)
             (not (and abdo-jabber-alert-ignore-user-regexp
                       (string-match abdo-jabber-alert-ignore-user-regexp (jabber-jid-displayname from)))))
    (abdo-notify-message "gtalk" (format "gtalk: %s" (jabber-jid-displayname from)) "contact is online")))


(defun abdo-jabber-message-notify (from buf text proposed-alert)
  (when (and (not (jabber-muc-sender-p from))
             (abdo-buffer-notify-p buf)
             (not (and abdo-jabber-alert-ignore-user-regexp
                       (string-match abdo-jabber-alert-ignore-user-regexp (jabber-jid-displayname from)))))
      (abdo-chat-notify "gtalk" from (jabber-jid-displayname from) text buf)))


(defun abdo-jabber-muc-notify (from group buf text proposed-alert)
  (when (and (jabber-muc-sender-p from)
             (abdo-buffer-notify-p buf)
             (and abdo-jabber-alert-keyword-regexp
                  (string-match abdo-jabber-alert-keyword-regexp text))
             (not (and abdo-jabber-alert-ignore-user-regexp
                       (string-match abdo-jabber-alert-ignore-user-regexp (jabber-jid-displayname (jabber-jid-user from))))))

    (abdo-chat-notify "gtalk" (jabber-jid-user from)
                      (jabber-jid-displayname (jabber-jid-user from))
                      text buf)))



(defun abdo-jabber-global-things ()

  ; paths
  (setq jabber-history-dir (concat abdo-chat-directory "emacs/jabber"))
  (setq jabber-avatar-cache-directory (concat abdo-chat-directory "emacs/avatars"))

  ; logs
  (setq jabber-history-enabled t)
  (setq jabber-use-global-history nil)
  (setq jabber-history-enable-rotation t)
  (setq jabber-history-size-limit 512)

  ; chatting settings
  (setq jabber-backlog-number 10)
  (setq jabber-backlog-days 3)

  (setq jabber-chat-local-prompt-format "%t %n: ")
  (setq jabber-chat-foreign-prompt-format "%t %n: ")
  (setq jabber-chat-system-prompt-format  "%t *** ")

  (setq jabber-chat-time-format "[%H:%M:%S]")
  (setq jabber-chat-delayed-time-format "[%Y-%m-%d %H:%M:%S]")

  (setq jabber-print-rare-time t)
  (setq jabber-rare-time-format "%a %e %b %Y %H:00")

  ; buffer names
  (setq jabber-browse-buffer-format "*browse-%n*")
  (setq jabber-chat-buffer-format "*gtalk-%n*")
  (setq jabber-groupchat-buffer-format "*gchat-%n*")
  (setq jabber-muc-private-buffer-format "*muc-%n*")

  ; roster buffer
  (setq jabber-roster-buffer "*google-talk*")
  (setq jabber-show-resources 'always)
  (setq jabber-show-offline-contacts nil)
  (setq jabber-roster-show-bindings nil)
  (setq jabber-roster-line-format   " %c ::%-30n %u %-15s")
  (setq jabber-resource-line-format "     - %-28r  %2p %-15s")


  ; notifications
  (setq jabber-message-alert-same-buffer nil)  ; don't display alerts for current buffer

  ; avatar settings
  (setq jabber-vcard-avatars-retrieve nil)
  (setq jabber-chat-buffer-show-avatar nil)
  (setq jabber-vcard-avatars-publish nil)

  ; default status
  (setq jabber-default-priority 30)  ; so that overrules web and mobile
  (setq jabber-default-show "")
  (setq jabber-default-status "")

  ; highlight urls
  (add-hook 'jabber-chat-mode-hook 'goto-address)

  ; account
  (setq jabber-account-list '())
  (let ((gtalk (netrc-machine (netrc-parse "~/.netrc") "talk.google.com")))
    (add-to-list 'jabber-account-list
          `(,(netrc-get gtalk "login")
             (:password . ,(netrc-get gtalk "password"))
             (:network-server . ,(netrc-get gtalk "machine"))
             (:port . 5223)
             (:connection-type . ssl)
             )))


  ; notifications
  (add-hook 'jabber-alert-message-hooks 'abdo-jabber-message-notify)
  (add-hook 'jabber-alert-muc-hooks 'abdo-jabber-muc-notify)
  (add-hook 'jabber-alert-presence-hooks 'abdo-jabber-presence-notify)

  ; other hooks
  (add-hook 'jabber-chat-mode-hook 'abdo-jabber-chat-mode-things)

  ; hide on connection.
  ; This hook is run before jabber-send-current-presence
  (add-hook 'jabber-post-connect-hook 'abdo-hide-jabber)
  ; (remove-hook 'jabber-post-connect-hook 'jabber-send-current-presence)

)

(defun abdo-jabber-chat-mode-things ()
  ;; Turn on spell checking.
  (flyspell-mode 1)
  (jabber-activity-mode 0)   ; I have my own, thanks
)

(defun abdo-open-jabber ()
  (interactive)
  (jabber-connect-all)
)

(defun abdo-close-jabber ()
  (interactive)
  (jabber-disconnect))


;; Jabber hide and unhide capabilities
;; Source: http://chinmaykamat.wordpress.com/2010/01/22/google-talk-invisible-mode-in-pidgin/

(defun abdo-hide-jabber (&optional con)
  (interactive)
  (message "Jabber: hiding")
  (jabber-ask-capabilities)             ; for some reason, without this it doesn't work
  (dolist (jc (if con '(con) jabber-connections))
    (let ((jid (jabber-connection-bare-jid jc)))
      (jabber-send-iq
       jc jid "set"
       '(query ((xmlns . "google:shared-status") (version . "2"))
              (invisible ((value . "true"))))
       nil nil nil "Setting invisibility failed")))
  (setq abdo-jabber-hidden t))

(defun abdo-unhide-jabber (&optional con)
  (interactive)
  (message "Jabber: unhiding")
  (dolist (jc (if con '(con) jabber-connections))
    (let ((jid (jabber-connection-bare-jid jc)))
      (jabber-send-iq
       jc jid "set"
       '(query ((xmlns . "google:shared-status") (version . "2"))
              (invisible ((value . "false"))))
       nil nil nil "Clearing invisibility failed")))
  (setq abdo-jabber-hidden nil))

(defun jabber-ask-capabilities (&optional con)
  (interactive)
  (dolist (jc (if con '(con) jabber-connections))
    (let ((jid (jabber-connection-bare-jid jc)))
      (jabber-send-iq
       jc jid "get"
       '(query ((xmlns . "google:shared-status")(version . "2")))
       nil nil
       nil "Investigation failed"))))


;      (jabber-send-sexp jc
;                        `(iq ((type . "set") (id . "ss-2"))
;                             (query ((xmlns . "google:shared-status") (version . "2"))
;                                    (invisible ((value . "true"))))))


;  (dolist (jc jabber-connections)
;    (let ((jid (jabber-connection-bare-jid jc)))
;      (jabber-send-iq
;       jc jid "get"
;       '(query ((xmlns . "google:shared-status")(version . "2")))
;       'jabber-process-data nil
;       'jabber-process-data "Investigation failed")))



;; rcirc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-rcirc-global-things ()

  ;; Change user info
  (setq rcirc-default-nick "abtwo")
  (setq rcirc-default-user-name "abtwo")
  (setq rcirc-default-full-name "")

  ;; Set automatic authentication
  (setq rcirc-authinfo '())

  ;; (add-to-list 'rcirc-authinfo '("localhost" bitlbee "abdo" "*****"))

  (let ((freenode (netrc-machine (netrc-parse "~/.netrc") "chat.freenode.net"))
        (gitter   (netrc-machine (netrc-parse "~/.netrc") "irc.gitter.im")))

    ;; Server alist
    (setq rcirc-server-alist
          `(("chat.freenode.net"
             :port 6697
             :encryption tls
             :nick "abtwo"
             :full-name "Abdó Roig"
             ;;     :channels ("#emacs" "#python" )
             )
            ("irc.gitter.im"
             :port 6697
             :encryption tls
             :user ,(netrc-get gitter "login")
             :password ,(netrc-get gitter "password")
             :full-name "Abdó Roig"
             )
            ))

    (setq rcirc-authinfo
          `(("chat.freenode.net"
             nickserv
             ,(netrc-get freenode "login")
             ,(netrc-get freenode "password")
             )
            ))
    )

  (setq rcirc-log-filename-function 'abdo-rcirc-generate-log-filename)

  ;; hooks
  (add-hook 'rcirc-mode-hook 'abdo-rcirc-mode-things)
  (add-hook 'rcirc-print-hooks 'abdo-rcirc-notify))

(defun abdo-open-rcirc ()
  (interactive)
  (rcirc nil))


(defun abdo-close-rcirc ()
  (interactive)
  (dolist (buf (buffer-list))
    (when (eq (with-current-buffer buf major-mode) 'rcirc-mode)
      (with-current-buffer buf (ignore-errors (rcirc-cmd-quit "bye")))
;      do not kill the buffer. otherwise we are asked to terminate the attached process due to
;      a racy situation.
;      (kill-buffer buf)
      )))


(defun abdo-rcirc-away (reason)
  (dolist (process (rcirc-process-list))
	(rcirc-send-string process (concat "AWAY :" reason))))


(defun abdo-rcirc-generate-log-filename  (process target)
  (if target
      (cond
       ((not (and abdo-rcirc-log-ignore-regexp
                  (string-match abdo-rcirc-log-ignore-regexp target)))
        (rcirc-generate-new-buffer-name process target))
       (t nil))
    (process-name process)))


(defun abdo-rcirc-notify (proc sender response target text)
  ;; TODO: check that target is not current window
  (let ((textclean (replace-regexp-in-string "[^[:graph:] ]" "" text))
        (buf (when target (rcirc-get-buffer proc target))))
    (when (and (abdo-buffer-notify-p buf)
               (not (and abdo-rcirc-alert-ignore-user-regexp
                         (string-match abdo-rcirc-alert-ignore-user-regexp sender))))
      (cond
       ((and (string= response "PRIVMSG")
             (not (rcirc-channel-p target))
             (not (string= sender (rcirc-nick proc))))

        (abdo-chat-notify "rcirc" sender sender textclean buf))

       ((and (not (string= response "PRIVMSG"))
             (not (string= sender (rcirc-nick proc)))
             (string-match abdo-rcirc-alert-keyword-regexp text))

        ; TODO: may want to use rcirc-short-buffer-name to get shorter buffer names!
        (abdo-chat-notify "rcirc" sender sender textclean buf))))))


(defun abdo-rcirc-mode-things ()
  ;; Turn on spell checking.
  (flyspell-mode 1)

  ;; Timestamps
  (setq rcirc-time-format "[%Y-%m-%d|%H:%M:%S] ")

  ;; Track
  ; (rcirc-track-minor-mode 1)

  ;; Some filtering
  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

  ;; Logging
  (setq rcirc-log-flag t)
  (setq rcirc-log-directory (concat abdo-chat-directory "emacs/rcirc/"))

  ;; colorize nicks
  (require 'rcirc-color)   ;; Crahses in daemon mode
)

(abdo-rcirc-global-things)
(abdo-jabber-global-things)
(abdo-twitter-global-things)

(provide 'abdo-chat)
