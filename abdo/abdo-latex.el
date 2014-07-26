(provide 'abdo-latex)


;; Latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun abdo-latex-mode-things()

  ;; I need this because I installed auctex myself to get synctex support
;  (load "auctex.el")
;  (load "preview-latex.el")

  (require 'dbus)
  (require 'calibre)

  ;; This one is on my emacs-lisp dir.
  (load "cdlatex.el")
  (setq-default indent-tabs-mode nil)            ;; No tabs on indent
  (set-fill-column 82)                           ;; In LaTeX I want it this way

  (outline-minor-mode)                           ;; Outline mode
  (turn-on-reftex)                               ;; RefTex
  ;(turn-on-cdlatex)                             ;; CDLatex
  (setq reftex-auto-recenter-toc t)              ;; Enable reftex auto recentering
  (setq compilation-auto-jump-to-first-error t)  ;; Auto jump on error
  (setq use-file-dialog nil)                     ;; Disable dialog asking for a file on
                                                 ;; errors where the filename field points
                                                 ;; to a missing file.

  (setq TeX-auto-local ".auto/")                 ;; Hide the TeX auto local dir.
  (setq TeX-parse-self t)                        ;; Enable parse on load.
  (setq TeX-auto-save t)                         ;; Autosave style info.

  (setq reftex-toc-shown nil)                    ;; Disable toc in reftex

  ;; Don't need this the way I sync with zathura

;  (setq TeX-source-correlate-method 'synctex)    ;; Synctex
;  (setq TeX-source-correlate-mode t)
;  (setq TeX-source-correlate-start-server t)     ;; Not any more :)

  ;; Don't ask for the compilation command
  (setq compilation-read-command nil)

  ;; Set PDF mode
  (TeX-PDF-mode t)

  ;; Setup D-bus interface with evince
  ;; (abdo-latex-dbus-evince-setup)
  ;; (abdo-latex-dbus-zathura-setup)

  ;; evince with dbus in viewers
  (add-to-list 'TeX-view-program-list '("evince-dbus" abdo-latex-evince-dbus-view))
  (add-to-list 'TeX-view-program-selection '(output-pdf "evince-dbus"))

  ;; Set outline mode headings order
  (setq outline-promotion-headings '("\\chapter" "\\section" "\\subsection"
     "\\subsubsection" "\\paragraph" "\\subparagraph"))

  ;;(setq TeX-view-program-list (quote
  ;;   (("okular" "okular --unique '%o#src:%n %b'"))))

  ;;(setq TeX-view-program-list (quote
  ;;   (("evince" "evince-dbus %o %n %b"))))

  ;; Enable fixme mode
;  (add-to-list 'fixme-modes 'latex-mode)
;  (setq fixme-mode t)

  ;; Adjustments
  (abdo-latex-personal-tweaks)                   ;; My latex adjustments

  ;; Delete trailing whitespaces before save
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)


  ;; ac-math sources for auto-complete
  (setq ac-sources
        (append '(ac-source-math-unicode
                  ac-source-math-latex
                  ac-source-latex-commands)
                ac-sources))
)


;; Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Latex personal stuff
(defun abdo-latex-personal-tweaks ()
  (interactive)

  ;; enable draftools active comments
  (setq LaTeX-syntactic-comments nil)
  ; (setq LaTeX-indent-comment-start-regexp "%!\\|%")  % only affects indentation, not filling

  ;; label prefix for comdiag
  (add-to-list 'LaTeX-label-alist '("comdiag" . LaTeX-equation-label))

  ;; add environments
  (add-to-list 'LaTeX-environment-list '("comdiag" LaTeX-env-label))
  (add-to-list 'LaTeX-environment-list '("align" LaTeX-env-label))

  ;; default environment
  (setq LaTeX-default-environment "equation")

  ;; do not fontify scripts and superscripts
  (setq font-latex-fontify-script nil)
  (setq font-latex-fontify-sectioning nil)

  ;; Keywords
  ;(add-to-list 'font-latex-match-warning-keywords '("\&"))

  ;; Math environments
  (add-to-list 'font-latex-math-environments "comdiag")
  (add-to-list 'font-latex-math-environments "comdiag*")
  (add-to-list 'font-latex-math-environments "align")
  (add-to-list 'font-latex-math-environments "align*")

  ;; Disables fill inside some environments
  (add-to-list 'LaTeX-indent-environment-list '("comdiag"))
  (add-to-list 'LaTeX-indent-environment-list '("comdiag*"))
  (add-to-list 'LaTeX-indent-environment-list '("tikzpicture"))
  (add-to-list 'LaTeX-indent-environment-list '("align*"))
  (add-to-list 'LaTeX-indent-environment-list '("align"))

  ;; Let reftex recognize comdiag as an equation.
  (setq reftex-label-alist '(("comdiag" ?e nil nil t)))
)


(defun abdo-latex-ispell-hook ()
  ;; Disabling spell checking on some parts. Unfortunately, this does not work
  ;; with flyspell, only with ispell mode.

  ;; Disabling spell checking for dollar-equations
  (setq ispell-skip-region-alist
    (append ispell-skip-region-alist
      '(("\\$" . "\\$")
       ("\\$\\$" . "\\$\\$")
      )
    )
  )

  ;; Disabling spell checking for latex keywords and environments
  (setq ispell-tex-skip-alists
    (list
      (append (car ispell-tex-skip-alists)
        '(("\\\\includegraphics" ispell-tex-arg-end)
          ("\\\\bibliography"    ispell-tex-arg-end)
          ("\\\\eqref"           ispell-tex-arg-end)
        )
      )
      (append (cadr ispell-tex-skip-alists)
        '(("equation\\*?" . "\\\\end[ \t\n]*{[ \t\n]*equation\\*?[ \t\n]*}")
          ("align\\*?" . "\\\\end[ \t\n]*{[ \t\n]*align\\*?[ \t\n]*}")
          ("comdiag\\*?" . "\\\\end[ \t\n]*{[ \t\n]*comdiag\\*?[ \t\n]*}")
          ("tikzpicture" . "\\\\end[ \t\n]*{[ \t\n]*tikzpicture[ \t\n]*}")
          ("array" ispell-tex-arg-end 1)
        )
      )
    )
  )
)

;; Some other tweaks I do not use
;;
;; (setq ispell-extra-args '("-t" "--sug-mode=ultra"))

;; That flyspell-babel only changed language for the document. Not for comments !
;; May be useful, but not right now.

;  (require 'flyspell-babel)           ;; Change language according to babel

  ;; Set the correspondence to ispell dictionaries
;  (setq flyspell-babel-to-ispell-alist
;   '(("english" "american")
;     ("catalan" "catalan")
;     ("spanish" "spanish")
;     ("german" "ngerman")))
;
;  (flyspell-babel-setup)



;; Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Compile the current buffer
(defun abdo-latex-compile()
  (interactive)
  (if (file-exists-p (concat default-directory "Makefile"))
      (compile "make -B pdf")
;      (if (file-exists-p (concat default-directory "out"))
;          (compile (format "make -B -k %sout/%s.pdf" default-directory (TeX-master-file)))
;        (compile (format "make -B -k %s/%s.pdf" default-directory (TeX-master-file))))
    (compile (format "pdflatex -file-line-error -interaction=nonstopmode -synctex=1 %s.tex" (TeX-master-file))))

    ;; Set the regexp for matching errors in the compilation buffer to gnu only.
    ;; Otherwise there was an odd matcher that picked pieces of latex it should
    ;; not.
    (with-current-buffer "*compilation*"
      (set (make-local-variable 'compilation-error-regexp-alist) '(gnu))))


;; Show compilation buffer
(defun abdo-latex-show-compilation()
  (interactive)
  (switch-to-buffer-other-window "*compilation*")
)


;; Make a diff tex.
(defun abdo-latex-make-diff(revision)
  (interactive "sRevision: ")
  (shell-command (format "papers diff %s" revision))
  (find-file-other-window (format "%s.diff.tex"
    (TeX-master-file))
  )
)

(defun abdo-latex-output ()
  (file-truename (concat default-directory (TeX-master-file (TeX-output-extension)))))


;; Auctex and CDLatex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/AUCTeX
;; http://www.emacswiki.org/emacs/CDLaTeX


;; Open or close *toc* buffer on the left
(defun abdo-latex-toggle-toc()
  (interactive)
  (if (eq reftex-toc-shown nil)
    (progn
      (setq reftex-toc-split-windows-horizontally t)
      (reftex-toc)
      (setq reftex-toc-split-windows-horizontally nil)
      (setq reftex-toc-shown t)
    )
    (progn
      (setq reftex-toc-shown nil)
      (delete-windows-on "*toc*")
    )
  )
)


;; Synctex interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abdo-latex-view ()
  (interactive)
  (let
    ((pdf (abdo-latex-output)))
    (abdo-latex-zathura-view pdf)
    ))


(defun abdo-latex-forward-sync ()
  (interactive)
  (let
      ((pdf (abdo-latex-output))
       (tex (buffer-file-name))
       (line (line-number-at-pos))
       (col  (current-column)))
    (message (format "synctex forward: %s %s %s %s" pdf tex line col))
    (abdo-latex-zathura-forward-sync pdf tex line col)
    ))


(defun abdo-latex-reverse-sync (tex line col)
  (interactive)
  (message (format "synctex reverse: %s %s %s" tex line col))
  (abdo-latex-zathura-reverse-sync tex line col))


(defun abdo-latex-dbus-setup ()
  (abdo-latex-dbus-zathura-setup))



;; Zathura sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; view pdf in zathura
(defun abdo-latex-zathura-view (pdf)
  (let
    ((editor (concat (getenv "EMACSCLIENT") " -e '(abdo-latex-reverse-sync \"%{input}\" %{line} %{column})'")))
    (call-process "zathura" nil 0 nil
                  "--synctex-editor-command"
                  editor
                  pdf)
    ))


;; zathura forward sync
(defun abdo-latex-zathura-forward-sync (pdf tex line col)
  (call-process "zathura" nil 0 nil
                (format "--synctex-forward=%s:%s:%s" line col tex)
                pdf)
  )


;; zathura reverse sync
(defun abdo-latex-zathura-reverse-sync (tex line col)
  (let
    ((buf (get-buffer (file-name-nondirectory tex)))
     (ret '(:boolean nil)))
    (if (null buf)
        (message "Sorry, %s is not opened..." tex)
      (switch-to-buffer buf)
      (goto-line line)
      (unless (= col -1) (move-to-column col))
      (setq ret '(:boolean t)))
    ret))


;; Setup D-bus interface for zathura reverse sync
(defun abdo-latex-dbus-zathura-setup()
  (when (and (eq window-system 'x) (fboundp 'dbus-register-signal))
    (message "Registering dbus method for zathura")

    ;; service
    ; (dbus-register-service :session "org.gnu.Emacs")

    ;; Reverse sync method
    (dbus-register-method :session "org.gnu.Emacs" "/synctex"
                          "org.gnu.Emacs" "reverse_sync"
                          'abdo-latex-zathura-reverse-sync)

    ;; Make stuff introspectable
    (dbus-register-method :session "org.gnu.Emacs" "/"
                          dbus-interface-introspectable "Introspect"
                          'abdo-latex-dbus-slash-introspect)

    (dbus-register-method :session "org.gnu.Emacs" "/synctex"
                          dbus-interface-introspectable "Introspect"
                          'abdo-latex-dbus-slash-synctex-introspect)

    ))


;; Introspection methods
(defun abdo-latex-dbus-slash-introspect ()
  "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"
  \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">
  <node name='/'>
    <interface name='org.freedesktop.DBus.Introspectable'>
    <method name='Introspect'>
    <arg name='xml_data' type='s' direction='out'/>
    </method>
    </interface>
    <node name='synctex'>
    </node>
  </node>")


(defun abdo-latex-dbus-slash-synctex-introspect ()
"<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"
  \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">
  <node name='/synctex'>
    <interface name='org.freedesktop.DBus.Introspectable'>
    <method name='Introspect'>
    <arg name='xml_data' type='s' direction='out'/>
    </method>
    </interface>
    <interface name='org.gnu.Emacs.synctex'>
      <method name='reverse_sync'>
        <arg name='tex' direction='in' type='s' />
        <arg name='line' direction='in' type='i' />
        <arg name='col' direction='in' type='i' />
        <arg name='ret' direction='out' type='b' />
      </method>
    </interface>
  </node>")




;; evince sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup D-bus interface with evince
(defun abdo-latex-dbus-evince-setup()
  (when (and (eq window-system 'x) (fboundp 'dbus-register-signal))
    (dbus-register-signal
      :session nil "/org/gnome/evince/Window/0"
      "org.gnome.evince.Window" "SyncSource"
      'abdo-latex-evince-reverse-sync
    )
  )
)


;; Handler for evince reverse search
;; Adapted from: http://www.mail-archive.com/auctex@gnu.org/msg04175.html

;; The optional timestamp is to mantain backwards compatibility.
;; Once all my systems run ubuntu 11.10, will be able to remove the &optional.
(defun abdo-latex-evince-reverse-sync (file linecol &optional timestamp)
  (let
    ((buf (get-buffer (file-name-nondirectory file)))
    (line (car linecol))
    (col (cadr linecol)))
    (if (null buf)
      (message "Sorry, %s is not opened..." file)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1) (move-to-column col))
    )
  )
;;  (sleep-for 0.5)                                    ;; Give time to release the click
;;  (select-frame-set-input-focus (selected-frame))    ;; Focus on the frame
)

;; Forward search.
;; Adapted from http://dud.inf.tu-dresden.de/~ben/evince_synctex.tar.gz

(defun abdo-latex-evince-forward-sync (pdffile texfile line)
  (let*
    ((dbus-name
      (dbus-call-method :session
        "org.gnome.evince.Daemon"  ; service
        "/org/gnome/evince/Daemon" ; path
        "org.gnome.evince.Daemon"  ; interface
        "FindDocument"
        (concat "file://" pdffile)
        t                          ; Open a new window if the file is not opened.
      ))
      (time (current-time))
      (high (car time))
      (low (cadr time))
      (timestamp (+ (* high (expt 2 16)) low)))

    (sleep-for 1)                  ; Need to let evince start
    (dbus-call-method :session
      dbus-name
      "/org/gnome/evince/Window/0"
      "org.gnome.evince.Window"
      "SyncView"
      texfile
      (list :struct :int32 line :int32 1)
      timestamp)

    ;; TODO: Try to set focus on evince
  )
)


;; View pdf in evince and sync current line via dbus
(defun abdo-latex-evince-dbus-view ()
  (interactive)
  (let
    ((pdf (file-truename (concat default-directory "out/" (TeX-master-file (TeX-output-extension)))))
    (tex (buffer-file-name))
    (line (line-number-at-pos)))
    (abdo-latex-evince-forward-sync pdf tex line)
  )
)



;; Setting the hooks and global stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'LaTeX-mode-hook 'abdo-latex-mode-things)
(add-hook 'abdo-ispell-hooks 'abdo-latex-ispell-hook)
