;;; clang-format.el --- Format code using clang-format

;; Copyright (C) 2014  Johann Klähn

;; Author: Johann Klähn <kljohann@gmail.com>
;; Keywords: tools, c

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

;;; Commentary:

;;

;;; Code:

(require 'json)

(defgroup clang-format nil
  "Format code using clang-format."
  :group 'tools)

(defcustom clang-format-executable
  (or (executable-find "clang-format") "clang-format")
  "Location of the clang-format executable."
  :group 'clang-format
  :type 'string)

(defcustom clang-format-style "file"
  "Style argument to pass to clang-format."
  :group 'clang-format
  :type 'string)

(make-variable-buffer-local 'clang-format-style)

;;;###autoload
(defun clang-format (start end &optional style)
  "Use clang-format to format the code between START and END according to STYLE.
If called interactively uses the region or the current buffer if there
is no active region.  If no style is given uses `clang-format-style'."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))

  (unless style
    (setq style clang-format-style))

  (let* ((discard-stderr '(t nil))
         (status
          (call-process-region
           (point-min) (point-max) clang-format-executable
           'delete discard-stderr nil

           "-assume-filename" (buffer-file-name)
           "-style" style
           "-offset" (number-to-string (1- start))
           "-length" (number-to-string (- end start))
           "-cursor" (number-to-string (1- (point)))))
         (json))

    (cond
     ((stringp status)
      (error "(clang-format killed by signal %s)" status))
     ((not (equal 0 status))
      (error "(clang-format failed with code %d)" status))
     (t (message "(clang-format succeeded)")))

    (goto-char (point-min))
    (setq json (json-read-from-string
                (buffer-substring-no-properties
                 (point-min) (line-end-position))))

    (delete-region (point-min) (line-beginning-position 2))
    (goto-char (1+ (cdr (assoc 'Cursor json))))))

(provide 'clang-format)
;;; clang-format.el ends here
