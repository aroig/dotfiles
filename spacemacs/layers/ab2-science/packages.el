(setq ab2-science-packages
      '(
        (sage-mode :location (recipe :fetcher bitbucket :repo "gvol/sage-mode" :files ("emacs/*.el")))
        (proofgeneral :location (recipe :fetcher github :repo "ProofGeneral/PG" :files ("*/*.el")))
        company-coq
        ))

(defun ab2-science/init-proofgeneral ()
  (use-package proof-site)

  ;; Some settings based on Arthur Malabara's blog post
  ;; http://endlessparentheses.com/proof-general-configuration-for-the-coq-software-foundations-tutorial.html
  (setq proof-splash-seen t
        proof-three-window-mode-policy 'hybrid
        proof-script-fly-past-comments t
        )
  )

(defun ab2-science/init-company-coq ()
  (add-hook 'coq-mode-hook #'company-coq-initialize))


(defun ab2-science/init-sage-mode ()
  (use-package sage-mode))

(defun coq/init-company-coq ()
  (use-package company-coq))
