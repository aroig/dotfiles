
;; Elisp paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Path of local emacs stuff
(setq abdo-emacs-directory  "/home/abdo/etc/emacs/")

;; Adds subdirectories at the begining of path
(let ((default-directory (concat abdo-emacs-directory "emacs-lisp/")))
  (setq load-path
    (append
      (let ((load-path (copy-sequence load-path))) ;; Shadow
        (append
          (copy-sequence (normal-top-level-add-to-load-path '(".")))
          (normal-top-level-add-subdirs-to-load-path)))
      load-path)))

;; theme paths
(let* ((theme-base (concat abdo-emacs-directory "themes/"))
       (theme-dirs (mapcar (lambda (d) (concat theme-base d))
                       (directory-files theme-base nil "[^.].*"))))
  (setq custom-theme-load-path (append theme-dirs custom-theme-load-path)))


;; Startup and window tweaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-screen t)                 ;; Disable startup screen
(menu-bar-mode -1)                              ;; Disable menubar

;; Default frame settings
(setq default-frame-alist '((menu-bar-lines . 0)
                            (tool-bar-lines . 0)))

;; Apply color theme
(load-theme 'zenburn t)
(require 'abdo-zenburn)


;; Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-directory (format "%s/" (getenv "AB2_WIKI_DIR")))       ;; Org mode paths
(setq abdo-mail-directory (format "%s/" (getenv "AB2_MAIL_DIR"))) ;; email

(setq abdo-personal-dicts-path "~/var/dicts/aspell/")             ;; dictionaries
(setq abdo-emacs-backups "~/var/bak/emacs/")                      ;; Backups dir
(setq bookmark-default-file "~/.emacs.d/bookmarks")               ;; bookmarks file

(setq abdo-chat-directory     "~/var/chat/")                      ;; chat logs
(setq abdo-download-directory "~/down/")                          ;; downloads


;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq abdo-user-full-name    "Abdó Roig-Maranges")
(setq abdo-user-mail-address "abdo.roig@gmail.com")
(setq abdo-name-and-mail (format "%s <%s>"
                                 abdo-user-full-name
                                 abdo-user-mail-address))


;; Machine specific configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let emacs know the machine's Hostname !

(setq system-name (substring (shell-command-to-string "hostname -s") 0 -1))
(setq full-system-name (substring (shell-command-to-string "hostname -f") 0 -1))


;; Autoload stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax highlighting
(autoload 'yaml-mode "yaml-mode" "Yaml editing mode." t)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(autoload 'coffee-mode "coffee-mode" "Coffee editing mode." t)
(autoload 'pkgbuild-mode "pkgbuild-mode" "PKGBUILD mode." t)
(autoload 'vala-mode "vala-mode" "Vala mode." t)
(autoload 'rainbow-mode "rainbow-mode" "Rainbow mode." t)
(autoload 'markdown-mode "markdown-mode.el" "Markdown files" t)
; (autoload 'sage-mode "sage-mode" "Sage mode." t)

;; Chat stuff
(autoload 'rcirc "rcirc" "Rcirc irc client." t)
(autoload 'twit "twittering-mode" "Twitter client." t)

;; Calibre
(autoload 'calibre-find "calibre" "Calibre interface." t)

;; haskell
(require 'haskell-mode-autoloads)


;; Loading stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic stuff
(require 'evil)                        ;; vi-like keys
(require 'abdo-modeline)               ;; modeline tweaks
(require 'ido)                         ;; Ido prompt
(require 'uniquify)                    ;; Make buffer names unique
(require 'ibuffer)                     ;; Nice buffer list
(require 'ibuffer-vc)                  ;; Nice buffer list of files under vc
(require 'undo-tree)                   ;; Undo tree
(require 'recentf)                     ;; Recent files
(require 'sensitive-mode)              ;; Mode for sensitive data (disable backups)
(require 'netrc)                       ;; read passwords from .netrc
(require 'sr-speedbar)                 ;; speedbar as a window

;; autocompletion
(require 'auto-complete-config)
(require 'ac-math)                     ;; produces warning (quotation)
(require 'yasnippet)                   ;; produces warning (cl-labels)

;; Helm
(require 'helm-config)
(require 'helm-misc)

;; Version Control
(when (locate-library "vc")
  (require 'vc))

(when (locate-library "magit")
  (require 'magit))

(when (locate-library "git-commit-mode")
  (require 'git-commit-mode)
  (require 'gitignore-mode)
  (require 'gitconfig-mode)
  (require 'git-rebase-mode))

;; email
(when (locate-library "mu4e")
  (require 'mu4e)                      ;; email client
  (require 'org-mu4e)                  ;; org and mu4e interaction
  (require 'helm-mu)                   ;; search email with helm

  (require 'abdo-mu4e)                 ;; personal mu4e stuff
)

;; org
(when (locate-library "org")
  (require 'org)                       ;; org
  (require 'org-mobile)                ;; orgmobile interaction
  (require 'org-id)                    ;; store message id's
  (require 'org-hacks)                 ;; let orgmobile handle symlinks

  (require 'abdo-org)                  ;; personal org stuff
)

;; sage
(when (locate-library "sage")
  (require 'sage)
  (require 'sage-view)
)

;; coq
(when (locate-library "coq")
  (require 'coq)
  (require 'abdo-coq)
)
(when (locate-library "proof-site")
  (require 'proof-site)
)

;; tex
(when (locate-library "tex-site")
  (require 'tex-site)                  ;; tex
  (require 'abdo-latex)                ;; personal latex stuff
)

;; Other personal stuff
(require 'abdo-vi)                     ;; Settings for vi mode
(require 'abdo-basic)                  ;; Basic emacs UI enhancements
(require 'abdo-edit)                   ;; Basic editing settings
(require 'abdo-languages)              ;; Spell checking stuff
(require 'abdo-utils)                  ;; Utility stuff
(require 'abdo-helm)                   ;; Personal helm stuff
(require 'abdo-chat)                   ;; Personal irc stuff
(require 'abdo-devel)                  ;; Personal devel stuff
(require 'abdo-keybindings)            ;; My personal keybindings
(require 'abdo-yaml)                   ;; Yaml mode things

;; Unsure if I need it anymore
;; (require 'cl)                          ;; Common lisp

;; parse vim modeline
(require 'vim-modeline)
(add-to-list 'find-file-hook 'vim-modeline/do)

(require 'tramp)                       ;; Tramp
;; Tramp uses ssh by default
(setq tramp-default-method "ssh")


;; Emacsdaemon stuff
;; TODO: load only when launching daemon!
(require 'abdo-emacsdaemon)


;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; haskell
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;; lua
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; vala
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))

;; coffee
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile$" . coffee-mode))

;; PKGBUILD's
(add-to-list 'auto-mode-alist '("PKGBUILD$" . pkgbuild-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; latex
(add-to-list 'auto-mode-alist '("\\.ltb$" . latex-mode))

;; coq
(add-to-list 'auto-mode-alist '("\\.v$" . coq-mode))


;; Command line switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'command-switch-alist '("diff" . abdo-command-line-diff))
(add-to-list 'command-switch-alist '("merge" . abdo-command-line-merge))
(add-to-list 'command-switch-alist '("org" . abdo-launch-org))
(add-to-list 'command-switch-alist '("notes" . abdo-launch-notes))
(add-to-list 'command-switch-alist '("mail" . abdo-launch-mail))
(add-to-list 'command-switch-alist '("chat" . abdo-launch-chat))
(add-to-list 'command-switch-alist '("sage" . abdo-launch-sage))

(defun abdo-launch-org (arg)
  (add-hook 'emacs-startup-hook 'abdo-org-main-buffer))

(defun abdo-launch-notes (arg)
  (add-hook 'emacs-startup-hook 'abdo-org-notes-buffer))

(defun abdo-launch-sage (arg)
  (sage))

(defun abdo-launch-chat (arg)
  (require 'jabber-autoloads)

  (abdo-chat-connect)
  (global-set-key (kbd "C-c C-x") 'abdo-chat-disconnect))

(defun abdo-launch-mail (arg)
  (abdo-mu4e))


;; Custom set stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
