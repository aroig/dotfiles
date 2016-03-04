
;; Old interface
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

(defun ab2/notify-message (who title msg)
  (start-process "notify" nil "notify-send" "-a" (format "%s" who)
                 (format "%s" title) (format "%s" msg)))


(defun ab2/urgency-hint (frame arg &optional source)
  (let* ((wm-hints (append (x-window-property
			    "WM_HINTS" frame "WM_HINTS" source nil t) nil))
	 (flags (car wm-hints)))
    (setcar wm-hints
	    (if arg
		(logior flags #x00000100)
	      (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))


(defun ab2/chat-notify (serv user nick text buf)
  "Notify a chat event"
  (message (format "%s: notification from %s" serv nick))
  (when
    (ab2/notify-message serv (format "%s: %s" serv nick) text)
    (ab2/urgency-hint (selected-frame) t)
    ; (when buf (abdo-modeline-buffer-alert buf nick))
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


(defun ab2/buffer-notify-p (buf)
  (or (not (memq (selected-window) (get-buffer-window-list buf)))       ; buffer not active
      (not (frame-has-focus-p (window-frame (get-buffer-window buf))))  ; frame doesn't have focus
      (not (eq (frame-visible-p (window-frame (selected-window))) t)))) ; frame is not visible




;; twitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ab2/twittering-mode-notify ()
  ;; loop over new tweets
  (dolist (el twittering-new-tweets-statuses)
    (let ((name (or (cdr (assoc 'user-name el)) ""))
          (user (or (cdr (assoc 'user-screen-name el)) ""))
          (text (or (cdr (assoc 'text el)) "")))

      (cond
       ;; notify mentions
       ((string-match (concat "@" (downcase twittering-username)) (downcase text))
        (ab2/chat-notify "twitter" name user text (get-buffer ":home")))

       ;; notify keyword match
       ((or (string-match ab2/twitter-alert-keyword-regexp text)
            (string-match ab2/twitter-alert-keyword-regexp user))
        (ab2/chat-notify "twitter" name user text (get-buffer ":home")))
       )))
)



;; jabber
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-jabber-presence-notify (from oldstatus newstatus statustext proposed-alert)
  (when (and (and abdo-jabber-alert-user-regexp
                  (string-match abdo-jabber-alert-user-regexp (jabber-jid-displayname from)))
             (eq oldstatus nil)
             (not (and abdo-jabber-alert-ignore-user-regexp
                       (string-match abdo-jabber-alert-ignore-user-regexp (jabber-jid-displayname from)))))
    (ab2/notify-message "gtalk" (format "gtalk: %s" (jabber-jid-displayname from)) "contact is online")))


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

(defun ab2/close-rcirc ()
  (interactive)
  (dolist (buf (buffer-list))
    (when (eq (with-current-buffer buf major-mode) 'rcirc-mode)
      (with-current-buffer buf (ignore-errors (rcirc-cmd-quit "bye")))
;      do not kill the buffer. otherwise we are asked to terminate the attached process due to
;      a racy situation.
;      (kill-buffer buf)
      )))


(defun ab2/rcirc-away (reason)
  (dolist (process (rcirc-process-list))
	(rcirc-send-string process (concat "AWAY :" reason))))


(defun ab2/rcirc-notify (proc sender response target text)
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
             (or (string-match abdo-rcirc-alert-keyword-regexp textclean)
                 (when (rcirc-channel-p target) (string-match abdo-rcirc-alert-channel-regexp target))))

        ; TODO: may want to use rcirc-short-buffer-name to get shorter buffer names!
        (abdo-chat-notify "rcirc" sender sender textclean buf))))))
