(provide 'abdo-languages)

;; Flyspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/FlySpell



(defun abdo-flyspell-things()
  ;; Need this?
  (require 'ispell)

  ;; Solve flyspell bug on maverick
  ;; http://stackoverflow.com/questions/1781762/enabling-flyspell-mode-gives-an-error
  ;(setq flyspell-issue-welcome-flag nil)

  ;; Use aspell instead of ispell. It's simply better !
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "list")


  ;; Dictionary list in my way
  ;; NOTE: it seems ispell-dictionary-alist is fine now
;  (setq ispell-dictionary-alist
;        '((nil "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d" "en") nil utf-8)
;          ("german" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "de") nil utf-8)
;          ("spanish" "[[:alpha:]]" "[^[:alpha:]]" "" t ("-d" "es") nil utf-8)
;          ("english" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d" "en") nil utf-8)
;          ("british" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d" "en_GB") nil utf-8)
;          ("american" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d" "en_US") nil utf-8)
;          ("catalan" "[[:alpha:]]" "[^[:alpha:]]" "['·-]" t ("-d" "ca") nil utf-8)
;          ("french" "[[:alpha:]]" "[^[:alpha:]]" "['·-]" t ("-d" "fr") nil utf-8)
;          ))

  ;; Don't ask to save personal dict
  (setq ispell-silently-savep t)

  ;; Run hooks
  (run-hooks 'abdo-ispell-hooks)
)

(defun abdo-change-dictionary (dictionary)
  "Changes the language for ispell together with my personal dictionary"
  (interactive (list (ido-completing-read "New Dictionary: " '("catalan" "english" "spanish" "german"))))
  (setq ispell-personal-dictionary (concat abdo-personal-dicts-path "abdo-" dictionary ".dict"))
  (ispell-change-dictionary dictionary))
