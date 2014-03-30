
;; Don't touch my modeline!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's monkey-patch the mode-line setup in helm.
(defun helm-display-mode-line (source)
  "Setup mode-line and header-line for `helm-buffer'."

  (set (make-local-variable 'helm-mode-line-string)
       (helm-interpret-value (or (and (listp source) ; Check if source is empty.
                                      (assoc-default 'mode-line source))
                                 (default-value 'helm-mode-line-string))
                             source))

  (let ((follow (and (eq (cdr (assq 'follow source)) 1) "(HF) ")))
    ;; Setup mode-line.
    (setq mode-line-process
          (format " %s" (helm-show-candidate-number
                         (when (listp helm-mode-line-string)
                           (car-safe helm-mode-line-string)))))

    ;; Setup header-line.
;    (let* ((hlstr (helm-interpret-value
;                   (and (listp source)
;                        (assoc-default 'header-line source)) source))
;           (hlend (make-string (max 0 (- (window-width) (length hlstr))) ? )))
;      (setq header-line-format
;            (propertize (concat " " hlstr hlend) 'face 'helm-header)))

))



;; Also helm-mu is involved in mode-line fuck-ups
(defun helm-mu-init ()
  "Initialize async mu process for `helm-source-mu'."
  (let ((process-connection-type nil)
        (maxnum (helm-candidate-number-limit helm-source-mu))
        (mucmd "mu find -f $'i\td\tf\tt\ts' --sortfield=d --maxnum=%d --reverse --format=sexp ")
        (sedcmd "sed -e ':a;N;$!ba;s/\\n\\(\\t\\|\\()\\)\\)/ \\2/g'"))
    (prog1
      (start-process-shell-command "helm-mu" helm-buffer
        (concat (format mucmd maxnum)
                (mapconcat 'shell-quote-argument
                           (split-string helm-pattern " ")
                           " ")
                 " | " sedcmd))
      (set-process-sentinel
        (get-buffer-process helm-buffer)
        #'(lambda (process event)
            (unless (string= event "finished\n")
              (helm-log "Error: Mu %s"
                        (replace-regexp-in-string "\n" "" event))))))))



(provide 'helm-hacks)
