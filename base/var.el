(which-key-mode t)

(setq delete-by-moving-to-trash t)
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)

;;;###autoload
(defun scame/dired-do-delete-permanently ()
  "Delete marked files in Dired without moving them to Trash."
  (interactive)
  (let ((delete-by-moving-to-trash nil)
        (trash-directory nil))
    (dired-do-delete)))

(save-place-mode t)

(global-auto-revert-mode t)
;; revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)

(delete-selection-mode 1)

(setq save-interprogram-paste-before-kill t)

;;;###autoload
(defun scame/convert-region-decimal-to-hexadecimal (start end)
  "Convert a region from decimal to hexadecimal."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((num (thing-at-point 'word)))
        (when (string-match-p "^[0-9]+$" num)
          (delete-region (point) (+ (point) (length num)))
          (insert (format "0x%x" (string-to-number num)))))
      (forward-word))))

;;;###autoload
(defun scame/convert-region-hexadecimal-to-decimal (start end)
  "Convert a region from hexadecimal to decimal."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((num (thing-at-point 'word)))
        (when (string-match-p "^0x[0-9a-fA-F]+$" num)
          (delete-region (point) (+ (point) (length num)))
          (insert (format "%d" (string-to-number (substring num 2) 16)))))
      (forward-word))))

(setq scroll-conservatively 0)

;;;###autoload
(defun add-compile-command (command)
  "add file variable with compile command"
  (interactive "sCommand: ")
  (add-file-local-variable-prop-line 'compile-command command))

(setq compilation-scroll-output 'first-error)

(add-hook 'prog-mode-hook #'which-function-mode)

(defmacro with-timer (name &rest body)
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06f seconds" ,name (float-time (time-since time)))))
;; usage:
;; (with-timer "description"
;; 	    (command))

(add-to-list 'auto-mode-alist '("\\Makefile\\..*" . makefile-gmake-mode))

;; (fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;;;###autoload
(defun scame/dired-strings-to-org ()
  "Recursively append file name and `strings` output for marked files in Dired to `strings.org`.
If a directory is marked, it processes all files within it recursively."
  (interactive)
  (let ((target-file (expand-file-name "strings.org" default-directory))
	(zwsp (string #x200B)))
    ;; Clear previous output file
    (ignore-errors (move-file-to-trash target-file))

    ;; Define the processor function to avoid code duplication
    (let ((process-file-fn
           (lambda (file)
             ;; only process regular files, skip pipes/sockets/subdirs
             (when (file-regular-p file)
               (with-temp-buffer
		 ;; Use relative path for the header so you know where the file came from
		 (insert (format "* %s\n" (file-relative-name file default-directory)))
		 (insert "#+BEGIN_SRC text\n")
		 (insert
                  (replace-regexp-in-string
                   "^\\*"
                   (concat zwsp "*")
                   (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process "strings" nil t nil file)))))
		 ;; Ensure newline before end_src
		 (unless (eq (char-before) ?\n) (insert "\n"))
		 (insert "#+END_SRC\n\n")
		 (append-to-file (point-min) (point-max) target-file))))))

      ;; Iterate over marked items
      (dolist (item (dired-get-marked-files))
	(if (file-directory-p item)
            ;; If it's a directory, walk it recursively
            (dolist (subfile (directory-files-recursively item ""))
              (funcall process-file-fn subfile))
          ;; If it's a file, process it directly
          (funcall process-file-fn item))))

    (message "Strings extracted to %s" target-file)))
