(provide 'abdo-languages)

;; Flyspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/FlySpell



(defun abdo-flyspell-things()
  ;; Need this?
  (require 'ispell)

  ;; Solve flyspell bug on maverick
  ;; http://stackoverflow.com/questions/1781762/enabling-flyspell-mode-gives-an-error
  (setq flyspell-issue-welcome-flag nil)

  ;; Use aspell instead of ispell. It's simply better !
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "list")

  ;; Enable flyspell mode
  (flyspell-mode 1)
)

(defun abdo-change-dictionary (dictionary)
  "Changes the language for ispell together with my personal dictionary"
  (interactive "MNew Dictionary: ")
  (setq ispell-personal-dictionary (concat abdo-personal-dicts-path "abdo-" dictionary ".dict"))
  (ispell-change-dictionary dictionary)
)

(eval-after-load "ispell"
  (progn
    ;; Run hooks
    (run-hooks 'abdo-ispell-hooks)

    ;; Dictionary list in my way
    (setq ispell-dictionary-alist
      '(("german" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "de") nil utf-8)
        ("spanish" "[[:alpha:]]" "[^[:alpha:]]" "" t ("-d" "es") nil utf-8)
        ("english" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d" "en") nil utf-8)
        ("british" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d" "en_GB") nil utf-8)
        ("american" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d" "en_US") nil utf-8)
        ("catalan" "[[:alpha:]]" "[^[:alpha:]]" "['·-]" t ("-d" "ca") nil utf-8)
        ("french" "[[:alpha:]]" "[^[:alpha:]]" "['·-]" t ("-d" "fr") nil utf-8)
      )
    )
    (setq ispell-dictionary "english")      ;; Default language
    (abdo-change-dictionary "english")
    (setq ispell-silently-savep t)          ;; Don't ask to save personal dict
  )
)

;; Enable flyspell on text modes
(add-hook 'text-mode-hook 'abdo-flyspell-things)




; (dolist (hook '(text-mode-hook))
;      (add-hook hook (lambda () (flyspell-mode 1))))
; (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
;      (add-hook hook (lambda () (flyspell-mode -1))))
