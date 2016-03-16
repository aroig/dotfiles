
;; Bugfix for helm-org. This is PR https://github.com/emacs-helm/helm/pull/1429
(with-eval-after-load "helm-org"
(defun helm-org--get-candidates-in-file (filename &optional fontify nofname parents)
  (with-current-buffer (pcase filename
                         ((pred bufferp) filename)
                         ((pred stringp) (find-file-noselect filename)))
    (let ((match-fn (if fontify
                        #'match-string
                      #'match-string-no-properties))
          (search-fn (lambda ()
                       (re-search-forward
                        org-complex-heading-regexp nil t)))
          (file (unless nofname
                  (concat (helm-basename filename) ":"))))
      (when parents
        (add-function :around (var search-fn)
                      (lambda (old-fn &rest args)
                                (when (org-up-heading-safe)
                                  (apply old-fn args)))))
      (save-excursion
        (save-restriction
          (widen)
          (unless parents (goto-char (point-min)))
          ;; clear cache for new version of org-get-outline-path
          (setq org-outline-path-cache nil)
          (cl-loop with width = (window-width (helm-window))
                   while (funcall search-fn)
                   for beg = (point-at-bol)
                   for end = (point-at-eol)
                   when (and fontify
                             (null (text-property-any
                                    beg end 'fontified t)))
                   do (jit-lock-fontify-now beg end)
                   for level = (length (match-string-no-properties 1))
                   for heading = (funcall match-fn 4)
                   if (and (>= level helm-org-headings-min-depth)
                           (<= level helm-org-headings-max-depth))
                   collect `(,(propertize
                               (if helm-org-format-outline-path
                                   (org-format-outline-path
                                    ;; org-get-outline-path changed in signature and behaviour since org's
                                    ;; commit 105a4466971. Let's fall-back to the new version in case
                                    ;; of wrong-number-of-arguments error.
                                    (condition-case nil
                                        (append (apply #'org-get-outline-path
                                                       (unless parents
                                                         (list t level heading)))
                                                (list heading))
                                      (wrong-number-of-arguments
                                       (org-get-outline-path t t)))
                                    width file)
                                   (if file
                                       (concat file (funcall match-fn  0))
                                       (funcall match-fn  0)))
                               'helm-real-display heading)
                              . ,(point-marker))))))))
    )


