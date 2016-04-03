(setq ab2-science-packages
      '(
        (proofgeneral :location (recipe :fetcher github :repo "ProofGeneral/PG"))
        company-coq
        ))

(defun ab2-science/init-proofgeneral ()
  (use-package proofgeneral)

  ;; Some settings based on Arthur Malabara's blog post
  ;; http://endlessparentheses.com/proof-general-configuration-for-the-coq-software-foundations-tutorial.html
  (setq proof-splash-seen t
        proof-three-window-mode-policy 'hybrid
        proof-script-fly-past-comments t
        )

  ;; TODO: Do it the spacemacs way
  (when (fboundp 'company-coq-initialize)
    (add-hook 'coq-mode-hook #'company-coq-initialize))
  )


(defun coq/init-company-coq ()
  (use-package company-coq))
