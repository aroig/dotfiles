
(defun ab2/mu4e-headers-narrow-to-tag (tag)
  (interactive (list (completing-read "Tag: " mu4e-tags-completion-list)))
  (when tag (mu4e-headers-search-narrow (format "tag:\"%s\"" tag))))


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
