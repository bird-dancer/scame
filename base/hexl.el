;;;###autoload
(defun scame/hexl-hex-string-to-integer (hex-string)
  "Return decimal integer for HEX-STRING.
 Accepts optional 0x or 0X prefix."
  (interactive "sHex number: ")
  (when (string-match "\\`0[xX]\\(.+\\)" hex-string)
    (setq hex-string (match-string 1 hex-string)))
  (let ((hex-num 0))
    (while (not (equal hex-string ""))
      (setq hex-num (+ (* hex-num 16)
                       (hexl-hex-char-to-integer (string-to-char hex-string))))
      (setq hex-string (substring hex-string 1)))
    hex-num))

;;;###autoload
(defun scame/hexl-goto-address (expr)
  "Goto address in hexl-mode.
 - Accepts hex literals (0x...).
 - Accepts arithmetic (e.g. 0x20 + 10).
 - Supports relative jumps with +N / -N.
 Examples:
   0x20       → absolute 0x20
   0x100 + 7  → absolute 0x107
   +0x10      → move forward 0x10 bytes
   -32        → move back 32 bytes"
  (interactive "sAddress (expression): ")
  (require 'calc)
  (let* ((cur (hexl-current-address))
         (relative (string-match-p "\\`[+-]" expr))
         ;; turn 0x... into decimal literals for calc
         (expr (replace-regexp-in-string
		"0x[0-9A-Fa-f]+"
		(lambda (s) (format "(%d)" (string-to-number (substring s 2) 16)))
		;; "0x[0-9A-Fa-f]+\\>"
		;; (lambda (s)
		;;   (format "(%d)"(string-to-number (replace-regexp-in-string "\\`0[xX]" "" s) 16)))
		expr))
         (val (string-to-number (calc-eval expr)))
         (addr (if relative (+ cur val) val)))
    (hexl-goto-address addr)))
(with-eval-after-load 'hexl
  (define-key hexl-mode-map (kbd "M-j") 'scame/hexl-goto-address))

;;;###autoload
(defun scame/hexl-copy-current-address ()
  (interactive)
  (let ((select-enable-clipboard t)
        (addr (hexl-current-address)))
    (kill-new (format "0x%08x" addr))
    (message "Copied address 0x%08x to clipboard" addr)))
(with-eval-after-load 'hexl
  (define-key hexl-mode-map (kbd "C-c w") 'scame/hexl-copy-current-address))
