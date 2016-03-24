
(setq ab2-chat-packages
      '(
        rcirc
        rcirc-color
        twittering-mode
        jabber
        persp-mode
        ))


(defun ab2-chat/pre-init-rcirc ()
  ;; need netrc to configure credentials
  (use-package netrc)

  ;; configuration
  (setq rcirc-default-nick "abtwo"
        rcirc-default-user-name "abtwo"
        rcirc-default-full-name ""
        rcirc-log-directory (format "%s/chat/emacs/rcirc/" (getenv "AB2_VAR_DIR")))

  ;; server and authentication
  (let ((freenode (netrc-machine (netrc-parse "~/.netrc") "chat.freenode.net"))
        (gitter   (netrc-machine (netrc-parse "~/.netrc") "irc.gitter.im")))

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
             :channels ("#OfflineIMAP/imapfw")
             )
            )

          rcirc-authinfo
          `(("chat.freenode.net"
             nickserv
             ,(netrc-get freenode "login")
             ,(netrc-get freenode "password")
             )
            ))
    )

  ;; hooks
  ; (add-hook 'rcirc-print-hooks 'abdo-rcirc-notify)
)

(defun ab2-chat/post-init-rcirc ()

  ;; Override upstream spacemacs function so it handles channels of the form blah/foo
      (defun rcirc-write-log (process sender response target text)
        (when rcirc-log-directory
          (when (not (file-directory-p rcirc-log-directory))
            (make-directory rcirc-log-directory))
          (with-temp-buffer
            ;; Sometimes TARGET is a buffer :-(
            (when (bufferp target)
              (setq target (with-current-buffer buffer rcirc-target)))
            ;; Sometimes buffer is not anything at all!
            (unless (or (null target) (string= target ""))
              ;; Print the line into the temp buffer.
              (insert (format-time-string "%Y-%m-%d %H:%M "))
              (insert (format "%-16s " (rcirc-user-nick sender)))
              (unless (string= response "PRIVMSG")
                (insert "/" (downcase response) " "))
              (insert text "\n")
              ;; Append the line to the appropriate logfile.
              (let ((coding-system-for-write 'no-conversion))
                (write-region (point-min) (point-max)
                              (concat rcirc-log-directory  (downcase target))
                              t 'quietly))))))


  )


(defun ab2-chat/post-init-twittering-mode ()
  (setq twittering-username "abroig"
        twittering-initial-timeline-spec-string
        '(":home"
          ;; ":direct_messages"
          ;; ":replies"
          ;; ":favorites"
          ;; ":search/emacs/"
          ;; "user_name/list_name")
          )
        twittering-timer-interval 300
        )

  (with-eval-after-load "twittering-mode"
    ;; authorization token
    (setq twittering-oauth-access-token-alist
          (read
           (with-temp-buffer
             (insert-file-contents
              (format "%s/etc/twittering-mode/oauth-token" (getenv "AB2_PRIV_DIR")))
             (buffer-string)))))


  ;; TODO: write a notification function
  ;; (add-hook 'twittering-new-tweets-hook 'abdo-twittering-mode-notify)
)

(defun ab2-chat/post-init-jabber ()
  ;; need netrc to configure credentials
  (use-package netrc)

  (setq
   jabber-history-dir (format "%s/chat/emacs/jabber/" (getenv "AB2_VAR_DIR"))
   jabber-avatar-cache-directory (format "%s/chat/emacs/avatars/" (getenv "AB2_VAR_DIR"))

   ;; logs
   jabber-history-enabled t
   jabber-use-global-history nil
   jabber-history-enable-rotation t
   jabber-history-size-limit 512

   ;; chatting settings
   jabber-backlog-number 10
   jabber-backlog-days 3

   ;; jabber-chat-local-prompt-format "%t %n: "
   ;; jabber-chat-foreign-prompt-format "%t %n: "
   ;; jabber-chat-system-prompt-format  "%t *** "

   ;; jabber-chat-time-format "[%H:%M:%S]"
   ;; jabber-chat-delayed-time-format "[%Y-%m-%d %H:%M:%S]"

   jabber-print-rare-time t
   jabber-rare-time-format "%a %e %b %Y %H:00"

  ;; buffer names
  jabber-browse-buffer-format "*browse-%n*"
  jabber-chat-buffer-format "*gtalk-%n*"
  jabber-groupchat-buffer-format "*gchat-%n*"
  jabber-muc-private-buffer-format "*muc-%n*"

  ;; roster buffer
  jabber-roster-buffer "*google-talk*"
  jabber-show-resources 'always
  jabber-show-offline-contacts nil
  jabber-roster-show-bindings nil
  jabber-roster-line-format   " %c ::%-40n %u %-15s"
  jabber-resource-line-format "     - %-28r  %2p %-15s"

  ;; notifications
  jabber-message-alert-same-buffer nil

  ;; avatar settings
  jabber-vcard-avatars-retrieve nil
  jabber-chat-buffer-show-avatar nil
  jabber-vcard-avatars-publish nil

  ;; default status
  jabber-default-priority 30   ; so that overrules web and mobile
  jabber-default-show ""
  jabber-default-status ""
  )

  ; highlight urls
  (add-hook 'jabber-chat-mode-hook 'goto-address)

  ;; account
  (let ((gtalk (netrc-machine (netrc-parse "~/.netrc") "talk.google.com")))
    (setq jabber-account-list
          `((,(netrc-get gtalk "login")
             (:password . ,(netrc-get gtalk "password"))
             (:network-server . ,(netrc-get gtalk "machine"))
             (:port . 5223)
             (:connection-type . ssl)
             ))))


  ;; notifications
  ;; TODO: fix this
  ;;(add-hook 'jabber-alert-message-hooks 'abdo-jabber-message-notify)
  ;;(add-hook 'jabber-alert-muc-hooks 'abdo-jabber-muc-notify)
  ;;(add-hook 'jabber-alert-presence-hooks 'abdo-jabber-presence-notify)

  ; other hooks
  (add-hook 'jabber-chat-mode-hook
            (lambda ()
                (flyspell-mode 1)
                (jabber-activity-mode 0)))

  ; hide on connection.
  ; This hook is run before jabber-send-current-presence
  ; (add-hook 'jabber-post-connect-hook 'abdo-hide-jabber)
  ; (remove-hook 'jabber-post-connect-hook 'jabber-send-current-presence)
  )


(defun ab2-chat/post-init-persp-mode ()
  (spacemacs|define-custom-layout
   "@chat"
   :binding "c"
   :body
   (progn
     (add-hook 'rcirc-mode-hook #'(lambda () (persp-add-buffer (current-buffer))))
     (add-hook 'twittering-mode-hook #'(lambda () (persp-add-buffer (current-buffer))))
     (add-hook 'jabber-chat-mode-hook #'(lambda () (persp-add-buffer (current-buffer))))
     (add-hook 'jabber-browse-mode-hook #'(lambda () (persp-add-buffer (current-buffer))))
     (add-hook 'jabber-roster-mode-hook #'(lambda () (persp-add-buffer (current-buffer))))
     (call-interactively 'rcirc)
     (call-interactively 'twittering-mode)
     (call-interactively 'jabber-connect-all)
     ))
  ;; do not save rcirc buffers
  (spacemacs|use-package-add-hook
   persp-mode
   :post-config
   (push (lambda (b) (with-current-buffer b
                       (or (eq major-mode 'rcirc-mode)
                           (eq major-mode 'twittering-mode))))
         persp-filter-save-buffers-functions))

  ;; setup a command line switch for mu4e perspective
  (add-to-list 'command-switch-alist '("chat" . (lambda (args) (spacemacs/custom-perspective-@chat))))
  )
