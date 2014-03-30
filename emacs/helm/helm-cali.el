;;; helm-cali.el --- Helm sources for searching calibre

;; Copyright (C) 2013 Abdó Roig-Maranges <abdo.roig@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-when-compile (require 'cl))
(require 'helm)

(defgroup helm-cali nil
  "Helm completion for cali."
  :group 'mu4e)

(defcustom helm-cali-default-search-string ""
  "A default search string for new searches."
  :group 'helm-cali
  :type  'string)


(defvar helm-cali-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c v") 'helm-cali-open-document)
    (define-key map (kbd "C-c e") 'helm-cali-view-metadata)
    map)
  "Keymap used in helm-mu.")


(defvar helm-source-cali
  '((name . "Search calibre documents")
    (candidates-process . helm-cali-init)
    (candidate-transformer . (helm-cali-candidate-parser
                              helm-cali-candidates-formatter))
    (delayed)
    (no-matchplugin)
    (nohighlight)
    (requires-pattern . 3)
    (persistent-action . helm-cali-persistent-action)
    (action . (("Open document" . helm-cali-open-document)))))


(defun helm-cali-init ()
  "Initialize async mu process for `helm-source-mu'."
  (let ((process-connection-type nil)
        (maxnum (helm-candidate-number-limit helm-source-mu))
        (mucmd "mu find -f $'i\td\tf\tt\ts' --sortfield=d --maxnum=%d --reverse --format=sexp ")
        (sedcmd "sed -e ':a;N;$!ba;s/\\n\\(\\t\\|\\()\\)\\)/ \\2/g'"))
    (prog1
      (start-process-shell-command "helm-mu" helm-buffer
        (concat (format mucmd maxnum)
                (mapconcat 'shell-quote-argument
                           (split-string helm-pattern " ")
                           " ")
                 " | " sedcmd))
      (set-process-sentinel
        (get-buffer-process helm-buffer)
        #'(lambda (process event)
            (if (string= event "finished\n")
                (with-helm-window
                  (setq mode-line-format
                        '(" " mode-line-buffer-identification " "
                          (line-number-mode "%l") " "
                          (:eval (propertize
                                  (format "[Mu Process Finish- (%s results)]"
                                          (max (1- (count-lines
                                                    (point-min) (point-max))) 0))
                                  'face 'helm-grep-finish))))
                  (force-mode-line-update))
                (helm-log "Error: Mu %s"
                          (replace-regexp-in-string "\n" "" event))))))))


(defun helm-cali-candidate-parser (candidates)
  "Parses the sexps obtained from mu find."
  (loop for i in candidates
        if (string= i "mu: no matches for search expression")
          collect i
        else
          collect (car (read-from-string i))))

(defun helm-cali-candidate-formatter (candidate)
  "Formats a candidate to look like entries in mu4e headers view."
  (let ((line " "))
    (dolist (f-w mu4e-headers-fields)
      (let ((field (car f-w))
            (width (cdr f-w))
            (val (mu4e-message-field candidate (car f-w))) (str))
        (setq str
          (case field
            (:subject
              (concat
                (mu4e~headers-thread-prefix (mu4e-message-field candidate :thread))
                val))
            ((:maildir :path) val)
            ((:to :from :cc :bcc) (mu4e~headers-contact-str val))
            (:from-or-to (mu4e~headers-from-or-to candidate))
            (:date (format-time-string mu4e-headers-date-format val))
            (:mailing-list (mu4e~headers-mailing-list val))
            (:human-date (mu4e~headers-human-date candidate))
            (:flags (propertize (mu4e~headers-flags-str val)
                      'help-echo (format "%S" val)))
            (:tags (propertize (mapconcat 'identity val ", ")))
            (:size (mu4e-display-size val))
            (t (mu4e-error "Unsupported header field (%S)" field))))
        (when str
          (setq line (concat line
              (if (not width) str
                (truncate-string-to-width str width 0 ?\s t)) " ")))))
    (propertize line 'face
      (let ((flags (mu4e-message-field candidate :flags)))
        (cond
          ((memq 'trashed flags) 'mu4e-trashed-face)
          ((memq 'draft flags)   'mu4e-draft-face)
          ((or
             (memq 'unread flags)
             (memq 'new flags))  'mu4e-unread-face)
          ((memq 'flagged flags) 'mu4e-flagged-face)
          ((memq 'replied flags) 'mu4e-replied-face)
          ((memq 'passed flags)  'mu4e-forwarded-face)
          (t                     'mu4e-header-face))))))

(defun helm-cali-candidates-formatter (candidates)
  "Formats the candidates."
  (if (equal candidates '("mu: no matches for search expression"))
      (list (propertize (car candidates) 'face 'mu4e-system-face))
    (loop for i in candidates
          for width = (save-excursion (with-helm-window (window-width)))
          for line = (helm-cali-candidate-formatter i)
          collect (cons (truncate-string-to-width line width) i))))


(defun helm-cali-open-document (candidate)
  "Open document in external viewer."
  (interactive)
  (helm-run-after-quit 'mu4e-headers-search helm-pattern))

(defun helm-cali-view-metadata (candidate)
  "Display metadata in a buffer."
  (mu4e-view-message-with-msgid (plist-get candidate :message-id)))

(defun helm-cali-persistent-action (candidate)
  (save-selected-window
    (helm-cali-open-document candidate))
  ;; Redisplay.
  (sit-for 0.1))


;;;###autoload
(defun helm-cali ()
  "Search for calibre entries. `helm-cali-default-search-string'
   will be used as initial query."
  (interactive)
  (let ((input (concat helm-cali-default-search-string " ")))
    (helm :sources 'helm-source-cali
          :buffer "*helm cali*"
          :full-frame t
          :keymap helm-cali-map
          :input input
          :candidate-number-limit 500)))


(provide 'helm-cali)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-cali.el ends here
