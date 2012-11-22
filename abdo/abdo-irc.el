

;; IRC global settings
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
                                   ,(netrc-get freenode "password")))))


(defun abdo-rcirc-bitlbee-connect ()
  (rcirc-connect "localhost" 6667 "abdo")
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
    (start-process "page-me" nil "notify-send" "-a" "rcirc"
                   "-i" "/usr/share/icons/Faenza/actions/scalable/im-message-new.svg"
                    "IRC notification" (replace-regexp-in-string "[^[:graph:] ]" "" msg)))
)


(abdo-rcirc-global-things)
(add-hook 'rcirc-mode-hook 'abdo-rcirc-mode-things)

(provide 'abdo-irc)
