;;; multi-mode.el --- support for multiple major modes

;; Copyright (C) 2003, 2004, 2007, 2009  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: languages, extensions, files
;; Created: Sept 2003
;; $Revision: 1.13 $
;; URL: http://www.loveshack.ukfsn.org/emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a framework for a sort of meta mode which provides support
;; for multiple major modes in different regions (`chunks') of a
;; buffer -- sort of.  Actually, multiple indirect buffers are used,
;; one per mode (apart from the base buffer, which has the default
;; mode).  A post-command hook selects the correct buffer for the mode
;; around point.  This is done on the basis of calling the elements of
;; `multi-chunk-fns' and taking the value corresponding to a start
;; position closest to point.

;; [I originally tried maintaining information about the local major
;; mode on a text property maintained by font-lock along with a
;; `point-entered' property to control changing the indirect buffer.
;; This worked less well for reasons I've forgotten, unfortunately.
;; The post-command hook seems to be efficient enough in the simple
;; cases I've tried, and is most general.]

;; Using indirect buffers ensures that we always have the correct
;; local buffer properties like the keymap and syntax table, including
;; the local variable list.  There are other implementations of this
;; sort of thing, e.g. for literate programming, but as far as I know,
;; they all work either by continually re-executing the major mode
;; functions or by swapping things like the keymap in and out
;; explicitly.  Using indirect buffers seems to be the right thing and
;; is at least potentially more robust, for instance for things like
;; specific subprocesses associated with the buffer for Ispell or
;; whatever.  (However, it has the potential confusion of there
;; actually being multiple buffers, even if they mostly act like one.
;; It also may interact badly with other modes using
;; `post-command-hook', as with Flyspell.)  Also, it's fairly simple.

;; Maybe it won't turn out to be the best approach, but it currently
;; seems to me to be better than the approach used by MMM mode
;; <URL:http://mmm-mode.sourceforge.net/>, and is a lot less code.  I
;; suspect doing the job any better would require fairly serious
;; surgery on buffer behaviour.  For instance, consider being able
;; programmatically to treat discontinuous buffer regions as
;; continuous according to, say, text properties.  That would work
;; rather like having multiple gaps which the most primitive movement
;; functions skip.

;; The other major difference to MMM mode is that we don't operate on
;; more-or-less static regions in different modes which need to be
;; re-parsed explicitly and/or explicitly inserted.  Instead they're
;; dynamically defined, so that random editing DTRT.

;; Things like font-lock and Imenu need to be done piecewise over the
;; chunks.  Some functions, such as indentation, should be executed
;; with the buffer narrowed to the current chunk.  This ensures they
;; aren't thrown by syntax in other chunks which could confuse
;; `parse-partial-sexp'.

;; The intention is that other major modes can be defined with this
;; framework for specific purposes, e.g. literate programming systems,
;; e.g. literate Haskell, mixing Haskell and LaTeX chunks.  See the
;; example of haskell-latex.el, which can activate the literate mode
;; just by adding to `haskell-mode-hook'.  The multiple modes can also
;; be chosen on the basis of the file contents, e.g. noweb.el.

;; Problems:
;; * C-x C-v in the indirect buffer just kills both buffers.  (Perhaps an
;;   Emacs bug.)
;; * Font-locking doesn't always work properly in Emacs 21, due to lack
;;   of `font-lock-dont-widen', e.g. see the commentary in
;;   haskell-latex.el.
;; * C-x C-w in Emacs 21 may change the mode when it shouldn't.
;; * Flyspell needs modifying so as not to cause trouble with this.
;;   For the moment we hack the Flyspell hook functions.
;; * The behaviour of the region can appear odd if point and mark are in
;;   regions corresponding to different modes, since they are actually in
;;   different buffers.  We really want point and marks to be shared among
;;   the indirect buffers.
;; * `view-file' doesn't work properly -- only the main buffer gets the
;;   minor mode.  Probably fixable using `view-mode-hook'.
;; * Doubtless facilities other than Imenu, Font Lock and Flyspell
;;   should be supported explicitly or fixed up to work with this,
;;   e.g. by narrowing to the chunk around commands.  There may be a
;;   need for more hooks or other support in base Emacs to help.
;; * Fontification currently doesn't work properly with modes which
;;   define their own fontification functions, at least the way nXML mode
;;   does it.  nXML seems to need hacking to get this right, maybe by
;;   converting it to use font-lock.  nXML is the only mode I know
;;   which defines its own `fontification-functions', although PSGML
;;   does its own fontification differently again.  Generally, any
;;   fontification which doesn't respect the narrowing in force will
;;   cause problems.  (One might think about `flet'ting `widen' to
;;   `ignore' around fontification functions, but that means they can't
;;   parse from a previous point, as things like nXML should
;;   legitimately be able to do.)

;; Todo:
;; * Provide a simple way to define multi-chunk functions by a series
;;   of delimiting regexps, similarly to MMM mode -- someone asked for
;;   that.
;; * Provide a means for code describing `island' chunks, which
;;   doesn't know about the base or other modes, to describe a chain
;;   of chunks in that mode for mapping over chunks.

;;; Code:

(require 'font-lock)
(require 'imenu)
(eval-when-compile (require 'advice))

(defvar multi-indirect-buffers-alist nil
  "Alist of direct and indirect buffers v. major modes.
Internal use.  Buffer local.")
(make-variable-buffer-local 'multi-indirect-buffers-alist)

(defvar multi-normal-fontify-function nil
  "Fontification function normally used by the buffer's major mode.
Internal use.  Buffer local.")
(make-variable-buffer-local 'multi-normal-fontify-function)

(defvar multi-normal-fontify-functions nil
  "Fontification functions normally used by the buffer's major mode.
Internal use.  Buffer local.")
(make-variable-buffer-local 'multi-normal-fontify-functions)

(defvar multi-indirect-buffer-hook nil
  "Hook run by `multi-install-mode' in each indirect buffer.
It is run after all the indirect buffers have been set up.")

(defvar multi-select-mode-hook nil
  "Hook run after a different mode is selected.")

(defvar multi-chunk-fns nil
  "List of functions to determine the modes of chunks.
Each takes a single arg, the position at which to find the mode.  It returns
a list (MODE START END).
Buffer local.")
(make-variable-buffer-local 'multi-chunk-fns)

(defsubst multi-base-buffer ()
  "Return base buffer of current buffer, or the current buffer if it's direct."
  (or (buffer-base-buffer (current-buffer))
      (current-buffer)))

(defvar multi-late-index-function nil
  "Original value of `imenu-create-index-function' for the buffer's mode.")
(make-variable-buffer-local 'multi-late-index-function)

(defvar multi-mode-alist nil
  "Alist of elements (MODE . FUNCTION) specifying a buffer's multiple modes.
MODE is a major mode and FUNCTION is a function used as an element of
`multi-chunk-fns' or nil.  Use nil if MODE is detected by another element
of the alist.")

(if (fboundp 'define-obsolete-variable-alias)
    (define-obsolete-variable-alias 'multi-alist 'multi-mode-alist)
  (make-obsolete-variable 'multi-alist 'multi-mode-alist))

;; See the commentary below.
(defun multi-hack-local-variables ()
  "Like `hack-local-variables', but ignore `mode' items."
  (let ((late-hack (symbol-function 'hack-one-local-variable)))
    (fset 'hack-one-local-variable
	  (lambda (var val)
	    (unless (eq var 'mode)
	      (funcall late-hack var val))))
    (unwind-protect
	(hack-local-variables)
      (fset 'hack-one-local-variable late-hack))))

(defun multi-install-mode (mode &optional chunk-fn base)
  "Add MODE to the multiple major modes supported by the current buffer.
CHUNK-FN, if non-nil, is a function to select the mode of a chunk,
added to the list `multi-chunk-fns'.  BASE non-nil means that this
is the base mode."
  (unless (memq mode multi-indirect-buffers-alist) ; be idempotent
    ;; This is part of a grim hack for lossage in AUCTeX, which
    ;; bogusly advises `hack-one-local-variable'.  This loses, due to
    ;; the way advice works, when we run `multi-hack-local-variables'
    ;; below -- there ought to be a way round this, probably with CL's
    ;; flet.  Any subsequent use of it then fails because advice has
    ;; captured the now-unbound variable `late-hack'...  Thus ensure
    ;; we've loaded the mode in advance to get any autoloads sorted
    ;; out.  Do it generally in case other modes have similar
    ;; problems.  [The AUCTeX stuff is in support of an undocumented
    ;; feature which is unnecessary and, anyway, wouldn't need advice
    ;; to implement.  Unfortunately the maintainer seems not to
    ;; understand the local variables mechanism and wouldn't remove
    ;; this.  To invoke minor modes, you should just use `mode:' in
    ;; `local variables'.]
    (if (eq 'autoload (car-safe (indirect-function mode)))
	(with-temp-buffer
	  (insert "Local Variables:\nmode: fundamental\nEnd:\n")
	  (funcall mode)
	  (hack-local-variables)))
    (let ((new-buffer (if base
			  (current-buffer)
			;; Perhaps the name uniquification should use
			;; the mode name somehow (without getting long).
			(make-indirect-buffer (current-buffer)
					      (generate-new-buffer-name
					       (buffer-name))))))
      (with-current-buffer (multi-base-buffer)
	(push (cons mode new-buffer) multi-indirect-buffers-alist)
	(let ((alist multi-indirect-buffers-alist)
	      (hook multi-indirect-buffer-hook)
	      (fns (if chunk-fn
		       (add-to-list 'multi-chunk-fns chunk-fn)
		     multi-chunk-fns))
	      (alist2 multi-mode-alist)
	      (file (buffer-file-name))
	      (base-name (buffer-name))
	      (coding buffer-file-coding-system)
	      (multi-mode t))	       ; The modes might examine this.
	  (with-current-buffer new-buffer
	    (unless (and base (eq mode major-mode))
	      (funcall mode))
	    ;; Now we can make it local:
	    (set (make-local-variable 'multi-mode) t)
	    ;; Use file's local variables section to set variables in
	    ;; this buffer.  (Don't just copy local variables from the
	    ;; base buffer because it may have set things locally that
	    ;; we don't want in the other modes.)  We need to prevent
	    ;; `mode' being processed and re-setting the major mode.
	    ;; It all goes badly wrong if `hack-one-local-variable' is
	    ;; advised.  The appropriate mechanism to get round this
	    ;; appears to be `ad-with-originals', but we don't want to
	    ;; pull in the advice package unnecessarily.  `flet'-like
	    ;; mechanisms lose with advice because `fset' acts on the
	    ;; advice anyway.
	    (if (featurep 'advice)
		(ad-with-originals (hack-one-local-variable)
		  (multi-hack-local-variables))
	      (multi-hack-local-variables))
	    ;; Indentation should first narrow to the chunk.  Modes
	    ;; should normally just bind `indent-line-function' to
	    ;; handle indentation.
	    (when indent-line-function ; not that it should ever be nil...
	      (set (make-local-variable 'indent-line-function)
		   `(lambda ()
		      (save-restriction
			(multi-narrow-to-chunk)
			(,indent-line-function)))))
	    ;; Now handle the case where the mode binds TAB directly.
	    ;; Bind it in an overriding map to use the local definition,
	    ;; but narrowed to the chunk.
	    (let ((tab (local-key-binding "\t")))
	      (when tab
		(make-local-variable 'minor-mode-map-alist)
		(push (cons multi-mode
			    (let ((map (make-sparse-keymap)))
			      (define-key map "\t"
				`(lambda ()
				   (interactive)
				   (save-restriction
				     (multi-narrow-to-chunk)
				     (call-interactively ',tab))))
			      map))
		      minor-mode-map-alist)))
	    (setq multi-normal-fontify-function
		  font-lock-fontify-region-function)
	    (set (make-local-variable 'font-lock-fontify-region-function)
		 #'multi-fontify-region)
	    (setq multi-normal-fontify-functions fontification-functions)
	    (setq fontification-functions '(multi-fontify))
	    ;; Don't let parse-partial-sexp get fooled by syntax outside
	    ;; the chunk being fontified.  (Not in Emacs 21.)
	    (set (make-local-variable 'font-lock-dont-widen) t)
	    (setq multi-late-index-function imenu-create-index-function)
	    (setq imenu-create-index-function #'multi-create-index
		  multi-indirect-buffer-hook hook)
	    ;; Kill the base buffer along with the indirect one; careful not
	    ;; to infloop.
	    (add-hook 'kill-buffer-hook
		      '(lambda ()
			 (setq kill-buffer-hook nil)
			 (kill-buffer (buffer-base-buffer)))
		      t t)
	    ;; This should probably be at the front of the hook list, so
	    ;; that other hook functions get run in the (perhaps)
	    ;; newly-selected buffer.
	    (add-hook 'post-command-hook 'multi-select-buffer nil t)
	    ;; Avoid the uniqified name for the indirect buffer in the
	    ;; mode line.
	    (setq mode-line-buffer-identification
		  (propertized-buffer-identification base-name))
	    ;; Fixme: Are there other things to copy?
	    (setq buffer-file-coding-system coding)
	    ;; For benefit of things like VC
	    (setq buffer-file-name file)
	    (vc-find-file-hook))
	  ;; Propagate updated values of the relevant buffer-local
	  ;; variables to the indirect buffers.
	  (dolist (x alist)
	    (if (car x)
		(with-current-buffer (cdr x)
		  (setq multi-chunk-fns fns)
		  (setq multi-indirect-buffers-alist alist)
		  (setq multi-mode-alist alist2)
		  (run-hooks 'multi-indirect-buffer-hook)))))))))

(defun multi-map-over-chunks (beg end thunk)
  "For all chunks between BEG and END, execute THUNK.
THUNK is a function of no args.  It is executed with point at the
beginning of the chunk and with the buffer narrowed to the chunk."
  (save-excursion
    (save-window-excursion
      (goto-char beg)
      (while (< (point) end)
	(multi-select-buffer)
	(save-restriction
	  (multi-narrow-to-chunk)
	  (funcall thunk)
	  (goto-char (point-max)))
	(unless (multi-next-chunk-start)
	  (goto-char (point-max)))))))

;; We need this for asynchronous fontification by jit-lock, even
;; though we're redefining `fontification-functions'.
(defun multi-fontify-region (beg end loudly)
  "Multi-mode font-lock fontification function.
Fontifies chunk-by chunk within the region.
Assigned to `font-lock-fontify-region-function'."
  (let* ((modified (buffer-modified-p))
	 (buffer-undo-list t)
	 (inhibit-read-only t)
	 (inhibit-point-motion-hooks t)
	 (inhibit-modification-hooks t)
	 deactivate-mark)
    (font-lock-unfontify-region beg end)
    (save-restriction
      (widen)
      (multi-map-over-chunks
       beg end (lambda ()
		 (if (and font-lock-mode font-lock-keywords)
		     (funcall multi-normal-fontify-function
			      (point-min) (point-max) loudly)))))
    ;; In case font-lock isn't done for some mode:
    (put-text-property beg end 'fontified t)
    (when (and (not modified) (buffer-modified-p))
      (set-buffer-modified-p nil))))

;; I'm not sure it's worth trying to support non-font-lock
;; fontification functions like this (see this file's commentary).
(defun multi-fontify (start)
  "Multi-mode fontification function.
Fontifies chunk-by-chunk within the region from START for up to
`multi-fontification-chunk-size' characters."
  (save-restriction
    (multi-narrow-to-chunk)
    (run-hook-with-args 'multi-normal-fontify-functions start)))

(defun multi-create-index ()
  "Create Imenu index alist for the currently-selected buffer.
Works piece-wise in all the chunks with the same major mode.
Assigned to `imenu-create-index-function'."
  (let ((selected-mode major-mode)
	imenu-alist			; accumulator
	last mode)
    (multi-map-over-chunks
     (point-min) (point-max)
     (lambda ()
       (if (eq major-mode selected-mode)
	   ;; Index this chunk and merge results with accumulator.
	   (dolist (elt (funcall multi-late-index-function))
	     (if (not (listp (cdr elt)))
		 (push elt imenu-alist)	; normal element
	       (let ((elt2 (assoc (car elt) imenu-alist))) ; submenu
		 ;; Fixme: Assumes only a single level of submenu.
		 (if elt2
		     (setcdr elt2 (append (cdr elt) (cdr elt2)))
		   (push elt imenu-alist))))))))
    imenu-alist))

(defun multi-next-chunk-start ()
  "Move to the start of the next chunk."
  (widen)
  (goto-char (nth 2 (multi-find-mode-at)))
  (unless (eobp)
    (forward-char)
    t))

(eval-when-compile (defvar syntax-ppss-last))

(defun multi-narrow-to-chunk ()
  "Narrow to the current chunk."
  (interactive)
  (if (boundp 'syntax-ppss-last)
      (setq syntax-ppss-last nil))
  (unless (= (point-min) (point-max))
    (apply #'narrow-to-region (cdr (multi-find-mode-at)))))

(defun multi-select-buffer ()
  "Select the appropriate (indirect) buffer corresponding to point's context."
  ;; It may help to catch errors here.  If there are context-dependent
  ;; errors, it may well work correctly when point changes, but if it
  ;; gets an error, it will be removed from post-command-hook and there
  ;; won't be useful debugging context anyway.
  (condition-case ()
      (let ((buffer (cdr (assoc (car (multi-find-mode-at))
				multi-indirect-buffers-alist))))
	(unless (eq buffer (current-buffer))
	  (let* ((point (point))
		 (window-start (window-start))
		 (visible (pos-visible-in-window-p))
		 (oldbuf (current-buffer)))
	    (when (buffer-live-p buffer)
	      (switch-to-buffer buffer)
	      (bury-buffer oldbuf)
	      (goto-char point)
	      ;; Avoid the display jumping around.
	      (when visible
		(set-window-start (get-buffer-window buffer t) window-start))
	      (unless (eq buffer oldbuf)
		(run-hooks 'multi-select-mode-hook))))))
    (error nil)))

(defvar multi-mode-list (list t t t))

(defsubst multi-make-list (mode start end)
  "Constructor for lists returned by elements of `multi-chunk-fns' &c.
Destructively modifies `multi-mode-list' to avoid consing in
`post-command-hook'."
  (setcar multi-mode-list mode)
  (setcar (cdr multi-mode-list) start)
  (setcar (cddr multi-mode-list) end)
  multi-mode-list)

;; It would be nice to cache the results of this on text properties,
;; but that probably won't work well if chunks can be nested.  In that
;; case, you can't just mark everything between delimiters -- you have
;; to consider other possible regions between them.  For now, we do
;; the calculation each time, scanning outwards from point.
(defun multi-find-mode-at (&optional pos)
  "Apply elements of `multi-chunk-fns' to determine major mode at POS.
Return a list (MODE START END), the value returned by the function in the
list for which START is closest to POS (and before it); i.e. the innermost
mode is selected.  POS defaults to point."
  (let ((fns multi-chunk-fns)
	(start (point-min))
	(mode (with-current-buffer (multi-base-buffer)
		major-mode))
	(end (point-max))
	(pos (or pos (point)))
	val)
    (save-restriction
      (widen)
      (dolist (fn multi-chunk-fns)
	(setq val (funcall fn pos))
	(if (and val (or (not mode)
			 (>= (nth 1 val) start)))
	    (setq mode (nth 0 val)
		  start (nth 1 val)
		  end (nth 2 val)))))
    (unless (and (<= start end) (<= pos end) (>= pos start))
      (error "Bad multi-mode selection: %s, %s"
	     (multi-make-list mode start end) pos))
    (if (= start end)
	(setq end (1+ end)))
    (multi-make-list mode start end)))

;; This was basically for testing, and isn't a reasonable thing to use
;; otherwise.

;; (define-derived-mode multi-mode fundamental-mode ""
;;   "Pseudo major mode controlling multiple major modes apparently in a buffer.
;; Actually maintains multiple views of the data in indirect buffers and
;; switches between them according to the context of point with a post-command
;; hook.  Depends on a specification of `multi-mode-alist' in file variables."
;;   ;; We need to do the work after file variables have been processed so
;;   ;; that we can use a specification of `multi-mode-alist'.
;;   (set (make-local-variable 'hack-local-variables-hook)
;;        #'multi-mode-install-modes))

(defun multi-mode-install-modes ()
  "Process `multi-mode-alist' and create the appropriate buffers."
  (if multi-mode-alist
      (let ((elt (pop multi-mode-alist)))
	(multi-install-mode (car elt) (cdr elt) t)
	(dolist (elt multi-mode-alist)
	  (multi-install-mode (car elt) (cdr elt))))
    (fundamental-mode)
    (error "`multi-mode-alist' not defined for multi-mode")))

;; In 21.3, Flyspell breaks things, apparently by getting an error in
;; post-command-hook and thus clobbering it.  In development code it
;; doesn't do that, but does check indirect buffers it shouldn't.  I'm
;; not sure exactly how this happens, but checking flyspell-mode in
;; the hook functions cures this.  For the moment, we'll hack this up.
;; (Let's not bring advice into it...)
(eval-after-load "flyspell"
  '(progn
     (defalias 'flyspell-post-command-hook
       `(lambda ()
	  ,(concat (documentation 'flyspell-post-command-hook)
		   "\n\n[Wrapped by multi-mode.]")
	  (if flyspell-mode
	   (funcall ,(symbol-function 'flyspell-post-command-hook)))))

     (defalias 'flyspell-pre-command-hook
       `(lambda ()
	  (concat (documentation 'flyspell-pre-command-hook)
		  "\n\n[Wrapped by multi-mode.]")
	  (if 'flyspell-mode
	      (funcall ,(symbol-function 'flyspell-pre-command-hook)))))))

;; This is useful in composite modes to determine whether a putative
;; major mode is safe to invoke.
(defun multi-mode-major-mode-p (value)
  "Heuristic for VALUE being a symbol naming a major mode.
Checks whether the symbol's documentation string starts with
\"Major mode \"."
  (and (symbolp value) (fboundp value)
       (let ((doc (documentation value)))
	 (and doc (string-match "\\`Major mode " doc) t))))
(provide 'multi-mode)

;;; multi-mode.el ends here
