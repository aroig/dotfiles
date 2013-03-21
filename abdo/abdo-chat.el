
;; Some functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-notify-message (who title msg)
  (start-process "page-me" nil "notify-send" "-a" who
                 "-i" "/usr/share/icons/Faenza/actions/scalable/im-message-new.svg"
                 title (replace-regexp-in-string "[^[:graph:] ]" "" msg)))


;; Chat global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rcirc
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
    (add-to-list 'rcirc-authinfo `(,(netrc-get freenode "server") nickserv
                                   ,(netrc-get freenode "login")
                                   ,(netrc-get freenode "password")))))


;; twittering
(defun abdo-twittering-global-things ()

)

;; jabber
(defun abdo-jabber-global-things ()
;  (setq jabber-history-enabled t)
;  (setq jabber-use-global-history nil)
;  (setq jabber-backlog-number 40)
;  (setq jabber-backlog-days 30)

  ; avatar settings
  (setq jabber-avatar-cache-directory "~/Documents/chat/avatars")
  (setq jabber-chat-buffer-show-avatar nil)
  (setq jabber-vcard-avatars-publish t)

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
;             (:port . 443)
;             (:connection-type . ssl)
             )))


  ; notifications
  (add-hook 'jabber-alert-message-hooks
            (lambda (from buf text proposed-alert)
              (when (or jabber-message-alert-same-buffer
                        (not (memq (selected-window) (get-buffer-window-list buf))))
                (if (jabber-muc-sender-p from)
                    (abdo-notify-message
                     "gtalk"
                     (format "Gtalk: %s" (jabber-jid-displayname (jabber-jid-user from)))
                     (format "%s: %s" (jabber-jid-resource from) text))
                  (abdo-notify-message
                   "gtalk"
                   (format "%s" (jabber-jid-displayname from))
                   text)))))
)



(defun abdo-rcirc-mode-things ()
  ; (require 'rcirc-hacks)
  (require 'rcirc-notify)  ;; Dunno what happens
  (require 'rcirc-color)   ;; Crahses in daemon mode

  ;; Turn on spell checking.
  (flyspell-mode 1)

  ;; Timestamps
  (setq rcirc-time-format "[%Y-%m-%d][%H:%M] ")

  ;; Track
  (rcirc-track-minor-mode 1)

  ;; Some filtering
  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

  ;; Logging
  (setq rcirc-log-flag t)
  (setq rcirc-log-directory "~/Documents/chat/rcirc/")

  ;; Notify function
  (defun my-page-me (msg)
    (abdo-notify-message "rcirc" "IRC Notification" msg))
)


(abdo-rcirc-global-things)
(abdo-twittering-global-things)
(abdo-jabber-global-things)

(add-hook 'rcirc-mode-hook 'abdo-rcirc-mode-things)

(provide 'abdo-chat)
