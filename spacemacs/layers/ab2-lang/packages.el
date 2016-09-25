(setq ab2-lang-packages
      '(
        google-translate
        ))

(defun ab2-lang/post-init-google-translate ()
  (setq google-translate-default-source-language "pl"
        google-translate-default-target-language "en"
        google-translate-translation-directions-alist
        '(("en" . "pl")
          ("pl" . "en")
          ("en" . "it")
          ("it" . "en")))
  (spacemacs/set-leader-keys
    "xge" 'ab2-lang/google-translate-to-english
    "xgp" 'ab2-lang/google-translate-to-polish
    "xgi" 'ab2-lang/google-translate-to-italian
    "xgd" 'ab2-lang/google-translate-to-german
    "xgs" 'ab2-lang/google-translate-to-spanish
    ))
