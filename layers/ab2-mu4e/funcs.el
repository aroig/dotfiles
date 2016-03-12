
;; Custom actions
;; -----------------------------------------------


(defvar mu4e-action-tags-completion-list '()
  "List of tags to show for autocompletion")


(defun ab2/action-retag-message (msg &optional retag-arg)
  "Change tags of a message. Accepts a comma-separated list of
   additions and removals.

   Example: +tag,+long tag,-oldtag

   would add 'tag' and 'long tag', and remove 'oldtag'."
  (let* (
      (path (mu4e-message-field msg :path))
	  (maildir (mu4e-message-field msg :maildir))
	  (oldtags (mu4e-message-field msg :tags))
      (tags-completion (append
                        mu4e-action-tags-completion-list
                        (mapcar (lambda (tag) (format "+%s" tag)) mu4e-action-tags-completion-list)
                        (mapcar (lambda (tag) (format "-%s" tag)) oldtags)))
      (retag (if retag-arg
                 (split-string retag-arg ",")
               (completing-read-multiple "Tags: " tags-completion)))
	  (header  mu4e-action-tags-header)
	  (sep     (cond ((string= header "Keywords") " ")
		     ((string= header "X-Label") " ")
		     ((string= header "X-Keywords") ", ")
		     (t ", ")))
	  (taglist (if oldtags (copy-sequence oldtags) '()))
	  tagstr)
    (dolist (tag retag taglist)
      (cond
	((string-match "^\\+\\(.+\\)" tag)
	  (setq taglist (push (match-string 1 tag) taglist)))
	((string-match "^\\-\\(.+\\)" tag)
	  (setq taglist (delete (match-string 1 tag) taglist)))
	(t
	  (setq taglist (push tag taglist)))))

    (setq taglist (sort (delete-dups taglist) 'string<))
    (setq tagstr (mapconcat 'identity taglist sep))

    (setq tagstr (replace-regexp-in-string "[\\&]" "\\\\\\&" tagstr))
    (setq tagstr (replace-regexp-in-string "[/]"   "\\&" tagstr))

    (if (not (mu4e~contains-line-matching (concat header ":.*") path))
      ;; Add tags header just before the content
      (mu4e~replace-first-line-matching
	"^$" (concat header ": " tagstr "\n") path)

      ;; replaces keywords, restricted to the header
      (mu4e~replace-first-line-matching
	(concat header ":.*")
	(concat header ": " tagstr)
       path))

    (mu4e-message (concat "tagging: " (mapconcat 'identity taglist ", ")))
    (mu4e-refresh-message path maildir)))

