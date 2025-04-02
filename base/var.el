(which-key-mode t)

(setq delete-by-moving-to-trash t)

(save-place-mode t)

(global-auto-revert-mode t)
;; revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)

(delete-selection-mode 1)

(setq save-interprogram-paste-before-kill t)

(defun scame-convert-region-decimal-to-hexadecimal (start end)
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

(defun scame-convert-region-hexadecimal-to-decimal (start end)
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

(setq compilation-scroll-output 'first-error)

(add-hook 'prog-mode-hook #'which-function-mode)

(fset 'yes-or-no-p 'y-or-n-p)
