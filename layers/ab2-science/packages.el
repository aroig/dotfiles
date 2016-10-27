(setq ab2-science-packages
      '(
        (sage-mode :location (recipe :fetcher bitbucket :repo "gvol/sage-mode" :files ("emacs/*.el")))
        (proofgeneral :location (recipe :fetcher github :repo "ProofGeneral/PG" :files ("*/*.el")))
        ;; company-coq
        ))

(defun ab2-science/init-proofgeneral ()
  (use-package proof-site
    :mode ("\\.coq\\'" . coq-mode)
    )

  ;; Some settings based on Arthur Malabara's blog post
  ;; http://endlessparentheses.com/proof-general-configuration-for-the-coq-software-foundations-tutorial.html
  (setq
   proof-splash-seen t
   proof-three-window-mode-policy 'hybrid
   proof-script-fly-past-comments t
   )

  (spacemacs|diminish holes-mode "ⓗ" "h")

  (spacemacs/set-leader-keys-for-major-mode 'coq-mode
    "]" 'proof-assert-next-command-interactive
    "[" 'proof-undo-last-successful-command
    "." 'proof-goto-point)

  ;; disable purpose mode
  (add-hook 'coq-mode-hook (lambda () (purpose-mode -1)))
  )

(defun ab2-science/init-sage-mode ()
  (use-package sage-mode))

(defun ab2-science/init-company-coq ()
  (use-package company-coq
    :defer t
    :init (add-hook 'coq-mode-hook #'company-coq-mode))
  )
