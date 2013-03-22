
;; Some functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar abdo-chat-alert-keyword-regexp "abtwo\\|ab2\\|Abdó\\|Abdo\\|abdo\\|abdó")
(defvar abdo-chat-alert-ignore-user-regexp "NickServ")

(defun abdo-notify-message (who title msg)
  (start-process "notify" nil "notify-send" "-a" (format "%s" who)
                 "-i" "/usr/share/icons/Faenza/actions/scalable/im-message-new.svg"
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
  (message (format "Chat: notification from %s" nick))
  (when
    (abdo-notify-message serv (format "%s: %s" serv nick) text)
    (abdo-urgency-hint (selected-frame) t)
    (when buf (abdo-modeline-buffer-alert buf))
    ;; TODO: ding!
    ))



;; twitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; twittering
(defun abdo-twittering-global-things ()

)



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

  ; roster buffer
  (setq jabber-roster-buffer "*roster*")

  ; notifications
  (setq jabber-message-alert-same-buffer nil)

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
)

(defun abdo-jabber-chat-mode-things ()
  ;; Turn on spell checking.
  (flyspell-mode 1)
)


;; Source: http://chinmaykamat.wordpress.com/2010/01/22/google-talk-invisible-mode-in-pidgin/

(defun jabber-hide ()
  (interactive)
  (let* ((jc jabber-buffer-connection)
         (jid (jabber-connection-bare-jid jc)))
    (jabber-send-sexp jc
                      `(iq ((type . "set") (to . ,jid) (id . "ss-2"))
                           (query ((xmlns . "google:shared-status") (version . "2"))
                                  (invisible ((value . "true"))))))))

(defun jabber-unhide ()
  (interactive)
  (let* ((jc jabber-buffer-connection)
         (jid (jabber-connection-bare-jid jc)))
    (jabber-send-sexp jc
                      `(iq ((type . "set") (to . ,jid) (id . "ss-2"))
                           (query ((xmlns . "google:shared-status") (version . "2"))
                                  (invisible ((value . "false"))))))))



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
      '(("irc.freenode.net" :port 6697 :encryption tls
         :nick "abtwo" :full-name "Abdó Roig"
  ;;     :channels ("#emacs" "#python" )
         )))

  ;; Set automatic authentication
  (setq rcirc-authinfo '())

  ;; (add-to-list 'rcirc-authinfo '("localhost" bitlbee "abdo" "*****"))

  (let ((freenode (netrc-machine (netrc-parse "~/.netrc") "irc.freenode.net")))
    (add-to-list 'rcirc-authinfo `("irc.freenode.net" nickserv
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


(defun abdo-rcirc-notify (proc sender response target text)

  ;; TODO: check that target is not current window
  (let ((textclean (replace-regexp-in-string "[^[:graph:] ]" "" text)))
    (cond
     ((and (string= response "PRIVMSG")
           (not (rcirc-channel-p target))
           (not (string= sender (rcirc-nick proc)))
           (not (string-match abdo-chat-alert-ignore-user-regexp sender)))
      (abdo-chat-notify "rcirc" sender sender textclean nil))

     ((and (not (string= response "PRIVMSG"))
           (not (string= sender (rcirc-nick proc)))
           (string-match abdo-chat-alert-keyword-regexp text)
           (not (string-match abdo-chat-alert-ignore-user-regexp sender)))
      (abdo-chat-notify "rcirc" sender sender textclean nil)))))


(defun abdo-rcirc-mode-things ()
  ;; Turn on spell checking.
  (flyspell-mode 1)

  ;; Timestamps
  (setq rcirc-time-format "[%Y-%m-%d][%H:%M] ")

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
(abdo-twittering-global-things)
(abdo-jabber-global-things)

(provide 'abdo-chat)
