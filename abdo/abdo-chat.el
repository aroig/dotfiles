
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-chat-status (status)
  (interactive (list (ido-completing-read "Status: " '("online" "away" "dnd"))))

  (cond
   ((string= status "online")
    (jabber-send-presence "" "" jabber-default-priority)
    (abdo-rcirc-away ""))

   ((string= status "away")
    (jabber-send-presence "away" "" jabber-default-priority)
    (abdo-rcirc-away "away"))

   ((string= status "away")
    (jabber-send-presence "dnd" "" jabber-default-priority)
    (abdo-rcirc-away "do not disturb"))

   ))


(defun abdo-chat-connect ()
  (interactive)
  (rcirc nil)
  (jabber-connect-all))


(defun abdo-chat-disconnect ()
  (interactive)
  (abdo-close-rcirc)
  (abdo-close-jabber))


;; Some functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar abdo-chat-alert-keyword-regexp "\\b\\(abtwo\\|ab2\\|Abdó\\|Abdo\\|abdo\\|abdó\\)\\b")
(defvar abdo-chat-alert-ignore-user-regexp "NickServ")

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



;; jabber
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun abdo-jabber-notify (from buf text proposed-alert)
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buf))))
    (cond
     ((and (jabber-muc-sender-p from)
           (string-match abdo-chat-alert-keyword-regexp text)
           (not (string-match abdo-chat-alert-ignore-user-regexp from)))
        (abdo-chat-notify "gtalk" (jabber-jid-user from)
                          (jabber-jid-displayname (jabber-jid-user from))
                          text buf))

     ((and (not (jabber-muc-sender-p from))
           (not (string-match abdo-chat-alert-ignore-user-regexp from)))
      (abdo-chat-notify "gtalk" from
                        (jabber-jid-displayname from)
                        text buf)))))


(defun abdo-jabber-global-things ()

  ; paths
  (setq jabber-history-dir "~/Documents/chat/emacs/jabber")
  (setq jabber-avatar-cache-directory "~/Documents/chat/emacs/avatars")

  ; logs
  (setq jabber-history-enabled t)
  (setq jabber-use-global-history nil)

  ; chatting settings
  (setq jabber-backlog-number 10)
  (setq jabber-backlog-days 3)

  (setq jabber-chat-local-prompt-format "%t %n: ")
  (setq jabber-chat-foreign-prompt-format "%t %n: ")
  (setq jabber-chat-system-prompt-format  "%t *** ")

  (setq jabber-chat-time-format "[%H:%M:%S]")
  (setq jabber-chat-delayed-time-format "[%Y-%m-%d][%H:%M:%S]")

  (setq jabber-print-rare-time t)
  (setq jabber-rare-time-format "%a %e %b %Y %H:00")

  ; buffer names
  (setq jabber-chat-buffer-format "*browse-%n*")
  (setq jabber-chat-buffer-format "*gtalk-%n*")
  (setq jabber-groupchat-buffer-format "*gtalk-%n*")
  (setq jabber-muc-private-buffer-format "*gtalk-%n*")

  ; roster buffer
  (setq jabber-roster-buffer "*google-talk*")
  (setq jabber-show-resources nil)
  (setq jabber-roster-line-format " %c %-30n %u %-15s")

  ; notifications
  (setq jabber-message-alert-same-buffer nil)  ; don't display alerts for current buffer

  ; avatar settings
  (setq jabber-vcard-avatars-retrieve nil)
  (setq jabber-chat-buffer-show-avatar nil)
  (setq jabber-vcard-avatars-publish nil)

  ; default status
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
             (:network-server . ,(netrc-get gtalk "server"))
             )))


  ; notifications
  (add-hook 'jabber-alert-message-hooks 'abdo-jabber-notify)
  (add-hook 'jabber-chat-mode-hook 'abdo-jabber-chat-mode-things)

  ; hide on connection.
  ; This hook is run before jabber-send-current-presence
  (add-hook 'jabber-post-connect-hook 'jabber-hide)
  ; (remove-hook 'jabber-post-connect-hook 'jabber-send-current-presence)

)

(defun abdo-jabber-chat-mode-things ()
  ;; Turn on spell checking.
  (flyspell-mode 1)
  (jabber-activity-mode 0)   ; I have my own, thanks
)

(defun abdo-close-jabber ()
  (interactive)
  (jabber-disconnect))


;; Source: http://chinmaykamat.wordpress.com/2010/01/22/google-talk-invisible-mode-in-pidgin/
(defun jabber-hide (&optional con)
  (interactive)
  (message "Jabber: hiding")
  (jabber-ask-capabilities)             ; for some reason, without this it doesn't work
  (dolist (jc (if con '(con) jabber-connections))
    (let ((jid (jabber-connection-bare-jid jc)))
      (jabber-send-iq
       jc jid "set"
       '(query ((xmlns . "google:shared-status") (version . "2"))
              (invisible ((value . "true"))))
       nil nil nil "Setting invisibility failed"))))

(defun jabber-unhide (&optional con)
  (interactive)
  (message "Jabber: unhiding")
  (dolist (jc (if con '(con) jabber-connections))
    (let ((jid (jabber-connection-bare-jid jc)))
      (jabber-send-iq
       jc jid "set"
       '(query ((xmlns . "google:shared-status") (version . "2"))
              (invisible ((value . "false"))))
       nil nil nil "Clearing invisibility failed"))))

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
  (interactive)
  ;; Change user info
  (setq rcirc-default-nick "abtwo")
  (setq rcirc-default-user-name "abtwo")
  (setq rcirc-default-full-name "")

  ;; Server alist
  (setq rcirc-server-alist
      '(("chat.freenode.net" :port 6697 :encryption tls
         :nick "abtwo" :full-name "Abdó Roig"
  ;;     :channels ("#emacs" "#python" )
         )))

  ;; Set automatic authentication
  (setq rcirc-authinfo '())

  ;; (add-to-list 'rcirc-authinfo '("localhost" bitlbee "abdo" "*****"))

  (let ((freenode (netrc-machine (netrc-parse "~/.netrc") "chat.freenode.net")))
    (add-to-list 'rcirc-authinfo `("chat.freenode.net" nickserv
                                   ,(netrc-get freenode "login")
                                   ,(netrc-get freenode "password"))))

  ;; hooks
  (add-hook 'rcirc-mode-hook 'abdo-rcirc-mode-things)
  (add-hook 'rcirc-print-hooks 'abdo-rcirc-notify))


(defun abdo-close-rcirc ()
  (interactive)
  (dolist (buf (buffer-list))
    (when (eq (with-current-buffer buf major-mode) 'rcirc-mode)
      (with-current-buffer buf (ignore-errors (rcirc-cmd-quit "bye")))
      (kill-buffer buf))))


(defun abdo-rcirc-away (reason)
  (dolist (process (rcirc-process-list))
	(rcirc-send-string process (concat "AWAY :" reason))))


(defun abdo-rcirc-notify (proc sender response target text)
  ;; TODO: check that target is not current window
  (let ((textclean (replace-regexp-in-string "[^[:graph:] ]" "" text))
        (buf (when target (rcirc-get-buffer proc target))))
    (cond
     ((and (string= response "PRIVMSG")
           (not (rcirc-channel-p target))
           (not (string= sender (rcirc-nick proc)))
           (not (string-match abdo-chat-alert-ignore-user-regexp sender)))
      (abdo-chat-notify "rcirc" sender sender textclean buf))

     ((and (not (string= response "PRIVMSG"))
           (not (string= sender (rcirc-nick proc)))
           (string-match abdo-chat-alert-keyword-regexp text)
           (not (string-match abdo-chat-alert-ignore-user-regexp sender)))
      ; TODO: may want to use rcirc-short-buffer-name to get shorter buffer names!
      (abdo-chat-notify "rcirc" sender sender textclean buf)))))


(defun abdo-rcirc-mode-things ()
  ;; Turn on spell checking.
  (flyspell-mode 1)

  ;; Timestamps
  (setq rcirc-time-format "[%Y-%m-%d][%H:%M:%S] ")

  ;; Track
  ; (rcirc-track-minor-mode 1)

  ;; Some filtering
  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

  ;; Logging
  (setq rcirc-log-flag t)
  (setq rcirc-log-directory "~/Documents/chat/emacs/rcirc/")

  ;; colorize nicks
  (require 'rcirc-color)   ;; Crahses in daemon mode
)

(abdo-rcirc-global-things)
(abdo-jabber-global-things)

(provide 'abdo-chat)
