
(defun ab2/mu4e-headers-narrow-to-tag (tag)
  (interactive (list (completing-read "Tag: " mu4e-tags-completion-list)))
  (when tag (mu4e-headers-search-narrow (format "tag:\"%s\"" tag))))


;; Retag action with completion.
(with-eval-after-load "mu4e"
  (defvar mu4e-action-tags-completion-list '()
    "List of tags to show for autocompletion in
  `mu4e-action-retag-message'.")

  (defun mu4e~contains-line-matching (regexp path)
    "Determine whether the file at path contains a line matching
   the given regexp."
    (with-temp-buffer
      (insert-file-contents path)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward regexp nil t)
            t
          nil))))

  (defun mu4e~replace-first-line-matching (regexp to-string path)
    "Replace the first line in the file at path that matches regexp
   with the string replace."
    (with-temp-file path
      (insert-file-contents path)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward regexp nil t)
            (replace-match to-string nil nil)))))

  (defun mu4e-action-retag-message (msg &optional retag-arg)
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
           (sep     (cond ((string= header "Keywords") ", ")
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
)


;; make count properties configurable

;; TODO
;; 1. share code paths between maildir and bookmarks
;; 2. Add a configuration list of fields and query modifier
;; 3. Make it into a minor mode. handle key bindings in a sane way.

(with-eval-after-load "mu4e-maildirs-extension"

  (defcustom mu4e-maildirs-extension-bookmark-fields '(:unread)
    "List of fields to update when reading counts.

Available fields:

:unread
:total"
    :group 'mu4e-maildirs-extension
    :type '(list))

  (defun mu4e-maildirs-extension-load-bookmarks ()
    "Fetch data or load from cache."
    (unless mu4e-maildirs-extension-bookmarks
      (mapc (lambda(it)
              (let ((query (nth 0 it))
                    (bm (list :data it)))
                (add-to-list 'mu4e-maildirs-extension-bookmarks bm t)
                (when (member :unread mu4e-maildirs-extension-bookmark-fields)
                  (mu4e-maildirs-extension-bm-count bm
                                                    :unread
                                                    (concat query " AND flag:unread")))
                (when (member :total mu4e-maildirs-extension-bookmark-fields)
                  (mu4e-maildirs-extension-bm-count bm :total query))))
            mu4e-bookmarks))
    mu4e-maildirs-extension-bookmarks))
