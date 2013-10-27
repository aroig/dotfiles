
;; Define magit's log-edit mode derived from git-commit-mode
(if (require 'git-commit nil 'noerror)
    (define-derived-mode magit-log-edit-mode git-commit-mode "Magit Log Edit"
      ;; Recognize changelog-style paragraphs
      (define-key magit-log-edit-mode-map (kbd "C-c C-s") 'git-commit-signoff)
      (set (make-local-variable 'paragraph-start)
	   (concat paragraph-start "\\|*\\|(")))
  (define-derived-mode magit-log-edit-mode text-mode "Magit Log Edit"
    ;; Recognize changelog-style paragraphs
    (set (make-local-variable 'paragraph-start)
	 (concat paragraph-start "\\|*\\|("))))


(provide 'vcs-hacks)