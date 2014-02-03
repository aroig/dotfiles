;; sudo.el -- sudo wrapper for saving / opening files
;; Author: Scott Vokes <scott@silentbicycle.com>
;; Version: 0.5 -- 2007-07-14
;;
;; Copyright (c) 2007 Scott Vokes
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;; Commentary:
;;  This provides the following commands:
;; * sudo-save-current-buffer - use sudo to chown a file owned by another user,
;;                              save, and then chown it back
;; * sudo-find-file - use sudo to open an otherwise unreadable file
;; * sudo-unset-ro-or-save - unset read-only (if necessary), else sudo-save.
;; * sudo-clear-cached-password - clear emacs cache of password
;; * sudo-kill-password-timeout - close the window in which sudo can be used
;;                                without a password, if normally required
;;                                (same as "sudo -K")
;;  _______________________________________ 
;; / Suggested usage:                      \
;; | bind C-x M-s to sudo-unset-ro-or-save |
;; \ bind C-x M-f to sudo-find-file        /
;;  --------------------------------------- 
;;         \   A__^
;;          \  (o#)\_______
;;             (__)\       )\/\
;;                 ||----w |
;;                 J|     |V
;;
;; TODO:
;; * integrate with dired, ffap, etc
;; * Use a hook to better deal with backups
;;
;; Please send patches to scott@silentbicycle.com .

(defvar sudo-keep-output-buffer nil
  "Whether to keep output buffer from sudo when there are no errors.")

(defvar sudo-cache-password nil
  "Whether to cache the sudo password or not.")

(defvar sudo-clear-password-always nil
  "Whether to automatically clear the user's password timeout,
requiring password entry every time.")

(defvar sudo-cached-password ""
  "The user's cached sudo password, or an empty string.")

(defun sudo-save-current-buffer ()
  "Save current buffer, running sudo if necessary."
  (interactive)
  (if (file-writable-p (buffer-file-name))
      (save-buffer)                     ;just save
    (progn
      (let ((old-owner-uid (nth 2 (file-attributes (buffer-file-name)))))
        (sudo-chown-file user-login-name (buffer-file-name))
        (save-buffer)
        (sudo-chown-file (int-to-string old-owner-uid) (buffer-file-name))
        (if sudo-clear-password-always
            (sudo-kill-password-timeout))))))

(defun sudo-find-file (fname)
  "Open a file, which may or may not be readable. If we can't
read it, call sudo, otherwise just find it as normal."
  (interactive "F(sudo) Find file: ")
  (if (file-readable-p fname)
      (find-file fname)                 ;then just open it
    (progn
      (let ((old-owner-uid (nth 2 (file-attributes fname))))
        (sudo-chown-file user-login-name (sudo-quoting fname))
        (sleep-for .25) ; give sudo process a chance to work
        (find-file fname)
        (sudo-chown-file (int-to-string old-owner-uid) (sudo-quoting fname))
        (rename-buffer (concat (buffer-name) "(sudo)"))
        (if sudo-clear-password-always
            (sudo-kill-password-timeout))))))

(defun sudo-unset-ro-or-save ()
  "Unset read-only flag for buffer, otherwise
sudo-save-current-buffer."
  (interactive)
  (if buffer-read-only
      ;(sudo-unset-read-only)
      (toggle-read-only)
    (sudo-save-current-buffer)))

(defun sudo-kill-password-timeout ()
  "Clobber the user's sudo password timeout."
  (interactive)
  (if (not (zerop (call-process "sudo" nil nil nil "-K")))
      (message "Problem clearing user's sudo password timeout.")))

(defun sudo-clear-cached-password ()
  "Clear cached password."
  (interactive)
  (setq sudo-cached-password ""))

(defun sudo-chown-file (user fname)
  "Chown a file to a given user, returning 0 on success or
raising an error."
  (if (not (zerop (sudo-process-wrapper (list "chown" user (sudo-quoting fname)))))
      (error "Error chowning %s to %s." fname user)
    0))

;; this is no longer used and may be deleted
(defun sudo-chmod-file (arg fname)
  "Chmod a file, using either an octal string (e.g., 644) or a
u+wx argument."
  (if (not (zerop (sudo-process-wrapper (list "chmod" arg (sudo-quoting fname)))))
      (error "Error chmod-ing %s to %s." fname arg)
    0))

(defun sudo-quoting (fname)
  "Do shell quoting, path expansion, etc."
  (shell-quote-argument (expand-file-name fname)))

(defun sudo-process-wrapper (args)
  "Call sudo with list of args, handling password query if necessary.
Returns nil when succesful, otherwise an error occured."
  (if (get-buffer "*sudo-output*")      ;if old buffer exists,
      (kill-buffer "*sudo-output*"))    ;kill it to not interfere with output
  (let ((process-connection-type nil))  ;just use a pipe
    (apply 'start-process `("sudo-process" "*sudo-output*" "sudo" ,@args)))
  (sleep-for .25)                      ;give sudo a split second to do its thing
  (let* ((sudo-process (get-process "sudo-process")) ;before we check its status
         (sudo-process-status (process-status sudo-process)))
    (cond ((or (null sudo-process-status) ;If sudo is done running...
               (string-equal sudo-process-status "exit"))
           (sudo-process-exited sudo-process)) ;did it go okay? cleanup etc
          ((string-equal sudo-process-status "run") ;still running, so
           (if (sudo-wants-password-p) ;does it want a password?
               (progn
                 (sudo-send-sudo-process-password sudo-process)
                 (while (eq (process-status sudo-process) "run")
                   (sleep-for .25))       ;wait for sudo to finish...
                                        ;FIXME can we avoid an infinite loop?
                 (sudo-process-exited sudo-process))
             (sudo-process-exited sudo-process)))
          (t (progn
               (message "Error with sudo wrapper.")
               1)))))

(defun sudo-process-exited (sudo-process)
  "Pass along the process exit status code (nil or 0 if no
error), cleaning up the output buffer unless there were problems
or sudo-keep-output-buffer is t."
  (let (sudo-process-exit-status (process-status sudo-process))
    (if (or (null sudo-process-exit-status)
            (zerop sudo-process-exit-status))
        (progn
          (unless sudo-keep-output-buffer
            (kill-buffer "*sudo-output*"))
          0)                            ;all went well, presumably
      (progn
        (message "Error in sudo process (%d)" sudo-process-exit-status)
        (switch-to-buffer "*sudo-output*")
        sudo-process-exit-status))))

(defun sudo-wants-password-p ()
  "Check output buffer to see if sudo is waiting for a password."
  (with-current-buffer "*sudo-output*"
    (goto-char (point-min))
    (if (search-forward "Password:" nil 1)
        t
      nil)))

(defun sudo-send-sudo-process-password (sudo-process)
  "Send the sudo process the user's password and an ENTER."
  (process-send-string sudo-process
                       (concat
                        (sudo-get-password)
                        "")))
  
(defun sudo-get-password ()
  "Prompt for sudo password, caching it if desired."
  (if sudo-cache-password
      (if (string-equal sudo-cached-password "")
          (setq sudo-cached-password
                (read-passwd "sudo password: "))
        sudo-cached-password)
    (read-passwd "sudo password: ")))

(provide 'sudo)
