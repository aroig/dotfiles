
;; Emacs paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; put server sockets under XDG_RUNTIME_DIR
(setq server-socket-dir (format "%s/emacs/" (getenv "XDG_RUNTIME_DIR")))

;; Path of local emacs stuff
(setq abdo-emacs-directory  (format "%s/emacs/" (getenv "AB2_CONF_DIR")))

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



;; Some global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Detect batch mode
(defconst batch-mode (or noninteractive (member "--batch" command-line-args))
  "True when running in batch mode.")

;; Detect daemon mode
(defconst daemon-mode (member "--daemon" command-line-args)
  "True when running daemon mode.")

;; Let emacs know the machine's hostname
(setq system-name (substring (shell-command-to-string "hostname -s") 0 -1))
(setq full-system-name (substring (shell-command-to-string "hostname -f") 0 -1))

;; My name and email
(setq abdo-user-full-name    "Abdó Roig-Maranges")
(setq abdo-user-mail-address "abdo.roig@gmail.com")
(setq abdo-name-and-mail (format "%s <%s>" abdo-user-full-name abdo-user-mail-address))



;; Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-directory (format "%s/" (getenv "AB2_WIKI_DIR")))                    ;; org mode
(setq abdo-mail-directory (format "%s/" (getenv "AB2_MAIL_DIR")))              ;; email
(setq abdo-papers-directory (format "%s/" (getenv "AB2_PAPERS_DIR")))          ;; papers

(setq abdo-chat-directory     (format "%s/chat/" (getenv "AB2_VAR_DIR")))      ;; chat logs

(setq abdo-personal-dicts-path (format "%s/usr/dict/aspell/" (getenv "HOME"))) ;; dictionaries
(setq bookmark-default-file "~/.emacs.d/bookmarks")                            ;; bookmarks file
(setq abdo-download-directory "~/down/")                                       ;; downloads

;; Semantic db
(setq semanticdb-default-save-directory (format "%s/.emacs.d/semanticdb/" (getenv "HOME")))

;; Backups
(setq abdo-emacs-backups (format "%s/.emacs.d/bak/" (getenv "HOME")))



;; Startup and window tweaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless batch-mode
  (setq inhibit-startup-screen t)                 ;; Disable startup screen
  (setq initial-scratch-message "")               ;; Clean scratch buffer

  (tool-bar-mode 0)                               ;; Disable toolbar
  (menu-bar-mode 0)                               ;; Disable menubar
  (scroll-bar-mode 0)                             ;; Disable scrollbar

  ;; Default frame settings
  (setq default-frame-alist '((menu-bar-lines . 0)
                              (tool-bar-lines . 0)))

  ;; Apply color theme
  (load-theme 'zenburn t)
  (require 'abdo-zenburn))



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
(autoload 'qml-mode "qml-mode" "QML mode." t)
; (autoload 'sage-mode "sage-mode" "Sage mode." t)

;; Calibre
(autoload 'calibre-find "calibre" "Calibre interface." t)

;; haskell
(require 'haskell-mode-autoloads)

;; CMake
(autoload 'cmake-mode "cmake-mode.el" "CMake mode" t)

;; Loading stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic stuff
(require 'sensitive-mode)                ;; Mode for sensitive data (disable backups)
(require 'netrc)                         ;; read passwords from .netrc
(require 'ansi-color)                    ;; tools to handle ansi escape sequences
(require 'cl)                            ;; Common lisp support

;; Undo-tree
(when (and (locate-library "undo-tree") (not batch-mode))
  (require 'undo-tree)                   ;; Undo tree
)

;; evil
(when (and (locate-library "evil") (not batch-mode))
  (require 'evil)                        ;; vi-like keys
  (require 'abdo-vi)                     ;; Settings for vi mode
)

;; tweak the modeline. needs evil
(when (and (locate-library "abdo-modeline") (not batch-mode))
  (require 'abdo-modeline)               ;; modeline tweaks
)

;; Basic interactive stuff
(unless batch-mode
  (require 'flx-ido)                     ;; better matching algorithm for ido
  (require 'ido)                         ;; Ido prompt
  (require 'uniquify)                    ;; Make buffer names unique
  (require 'ibuffer)                     ;; Nice buffer list
  (require 'ibuffer-vc)                  ;; Nice buffer list of files under vc
  (require 'recentf)                     ;; Recent files
  (require 'sr-speedbar)                 ;; speedbar as a window
)

;; autocompletion
(unless batch-mode
  (require 'auto-complete-config)
  (require 'ac-math)                     ;; produces warning (quotation)
  (require 'yasnippet)                   ;; produces warning (cl-labels)
)

;; Version Control
(when (locate-library "vc")
  (require 'vc))

;; git
(when (and (locate-library "magit") (not batch-mode))
  ;; NOTE 2014-06-24: magit causes "strinp nil" error when starting new emacs via emacsclient. WTF?!
  (require 'magit))

(when (and (locate-library "git-commit-mode") (not batch-mode))
  (require 'git-commit-mode)
  (require 'gitignore-mode)
  (require 'gitconfig-mode)
  (require 'git-rebase-mode))

;; helm
(when (and (locate-library "helm-config") (not batch-mode))
  (require 'helm-config)
  (require 'helm-misc)
  (require 'helm-mu)
  (require 'abdo-helm)
  (require 'helm-hacks))

;; email
(when (and (locate-library "mu4e") (not batch-mode))
  (require 'mu4e)                      ;; email client
  (require 'org-mu4e)                  ;; org and mu4e interaction
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
; (when (locate-library "coq")
;   (require 'coq)
;   (require 'abdo-coq)
; )
; (when (locate-library "proof-site")
;   (require 'proof-site)
; )

;; tex
(when (locate-library "tex-site")
  (require 'tex-site)                  ;; tex
  (require 'abdo-latex)                ;; personal latex stuff
)

;; Extempore
(when (locate-library "extempore")
  (require 'extempore)
  (require 'abdo-audio)
)

;; Supercollider sclang
(when (locate-library "sclang")
  (require 'sclang)
  (require 'abdo-audio)
)

;; personal settings
(require 'abdo-utils)                  ;; Utility stuff
(require 'abdo-basic)                  ;; Basic emacs settings
(require 'abdo-devel)                  ;; Personal devel stuff
(require 'abdo-text)                   ;; Text mode tweaks
(require 'abdo-music)                  ;; Music related modes

;; personal settings for interactive emacs
(unless batch-mode
  (require 'abdo-interactive)          ;; Basic settings for interactive emacs
  (require 'abdo-session)              ;; Session handling setup
  (require 'abdo-edit)                 ;; Basic editing settings
  (require 'abdo-languages)            ;; Spell checking stuff
  (require 'abdo-keybindings)          ;; My personal keybindings
)

;; parse vim modeline
(require 'vim-modeline)
(add-to-list 'find-file-hook 'vim-modeline/do)

;; Tramp
(unless batch-mode
  (require 'tramp)
  ;; Tramp uses ssh by default
  (setq tramp-default-method "ssh")
)

;; Emacsdaemon stuff
(when daemon-mode
  (require 'abdo-emacsdaemon)
)


;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; .h headers as c++
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; haskell
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;; lua
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; zsh
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

;; vala
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))

;; coffee
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile$" . coffee-mode))

;; PKGBUILD's
(add-to-list 'auto-mode-alist '("PKGBUILD$" . pkgbuild-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdi$" . markdown-mode))

;; latex
(add-to-list 'auto-mode-alist '("\\.ltb$" . latex-mode))

;; coq
(add-to-list 'auto-mode-alist '("\\.v$" . coq-mode))

;; CMake
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;; qml
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

;; systemd
(add-to-list 'auto-mode-alist '("\\.\\(service\\|target\\|scope\\|slice\\|timer\\)$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.\\(link\\|network\\|netdev\\)$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.\\(mount\\|automount\\|socket\\|device\\)$" . conf-mode))

;; mr
(add-to-list 'auto-mode-alist '("\\.mrconfig$" . conf-mode))

;; disable backups for some files
(add-to-list 'auto-mode-alist '("\\.gpg$" . sensitive-mode))
(add-to-list 'auto-mode-alist '("\\.ido\\.last" . sensitive-mode))
(add-to-list 'auto-mode-alist '("\\.recentf" . sensitive-mode))

(add-to-list 'auto-mode-alist `(,(abdo-escape-regexp (getenv "AB2_PRIV_DIR")) . sensitive-mode))
(add-to-list 'auto-mode-alist `(,(abdo-escape-regexp (file-truename "~/.ssh")) . sensitive-mode))

;; extempore
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

;; sclang
(add-to-list 'auto-mode-alist '("\\.scd$" . sclang-mode))



;; Command line switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'command-switch-alist '("diff"   . abdo-command-line-diff))
(add-to-list 'command-switch-alist '("merge"  . abdo-command-line-merge))
(add-to-list 'command-switch-alist '("git"    . abdo-launch-magit))
(add-to-list 'command-switch-alist '("org"    . abdo-launch-org))
(add-to-list 'command-switch-alist '("notes"  . abdo-launch-notes))
(add-to-list 'command-switch-alist '("mail"   . abdo-launch-mail))
(add-to-list 'command-switch-alist '("chat"   . abdo-launch-chat))
(add-to-list 'command-switch-alist '("sage"   . abdo-launch-sage))
(add-to-list 'command-switch-alist '("sclang" . abdo-launch-sclang))
(add-to-list 'command-switch-alist '("extempore" . abdo-launch-extempore))

(defun abdo-launch-magit (arg)
  (abdo-vcs-log)
  (delete-other-windows (selected-window))
  (abdo-vcs-status)
)

(defun abdo-launch-org (arg)
  (add-hook 'emacs-startup-hook 'abdo-org-main-buffer)

  ;; start emacs server with socket 'org'
  (setq server-name "org")
  (server-start))

(defun abdo-launch-notes (arg)
  (add-hook 'emacs-startup-hook 'abdo-org-notes-buffer)

  ;; start emacs server with socket 'notes'
  (setq server-name "notes")
  (server-start))

(defun abdo-launch-sage (arg)
  (sage))

(defun abdo-launch-sclang (arg)
  (sclang-start))

(defun abdo-launch-extempore (arg)
  (extempore-mode))

(defun abdo-launch-chat (arg)
  (require 'abdo-chat)
  (require 'jabber-autoloads)
  (require 'rcirc)
  (require 'twittering-mode)

  ;; load chat settings
  (load "~/var/chat/emacs/settings.el")

  ;; start emacs server with socket 'chat'
  (setq server-name "chat")
  (server-start)

  ;; Connect to chat and set disconnect bindings
  (abdo-chat-connect)
  (global-set-key (kbd "C-c C-x") 'abdo-chat-disconnect))

(defun abdo-launch-mail (arg)

  ;; start emacs server with socket 'mail'
  (setq server-name "mail")
  (server-start)

  ;; launch mu4e
  (abdo-mu4e))


;; Custom set stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
