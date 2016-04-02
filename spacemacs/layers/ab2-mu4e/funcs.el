
(defun ab2/mu4e-ask-tag (prompt)
  (let ((prompt (mu4e-format "%s" prompt)))
    (completing-read prompt mu4e-tags-completion-list)))


(defun ab2/mu4e-headers-jump-to-tag (tag)
  (interactive
   (let ((tag (ab2/mu4e-ask-tag "Jump to tag: ")))
     (list tag)))
  (when tag
    (mu4e-mark-handle-when-leaving)
    (mu4e-headers-search
     (format "tag:\"%s\"" tag))))

