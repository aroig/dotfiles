
(defun ab2-lang/google-translate-to-english (&optional override-p)
  (interactive "P")
  (let ((google-translate-default-source-language "auto")
        (google-translate-default-target-language "en"))
    (google-translate-at-point override-p)))

(defun ab2-lang/google-translate-to-polish (&optional override-p)
  (interactive "P")
  (let ((google-translate-default-source-language "auto")
        (google-translate-default-target-language "pl"))
    (google-translate-at-point override-p)))

(defun ab2-lang/google-translate-to-italian (&optional override-p)
  (interactive "P")
  (let ((google-translate-default-source-language "auto")
        (google-translate-default-target-language "it"))
    (google-translate-at-point override-p)))

(defun ab2-lang/google-translate-to-german (&optional override-p)
  (interactive "P")
  (let ((google-translate-default-source-language "auto")
        (google-translate-default-target-language "de"))
    (google-translate-at-point override-p)))

(defun ab2-lang/google-translate-to-spanish (&optional override-p)
  (interactive "P")
  (let ((google-translate-default-source-language "auto")
        (google-translate-default-target-language "es"))
    (google-translate-at-point override-p)))

