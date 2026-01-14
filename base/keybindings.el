;;;###autoload
(defun scame/kill-buffer-and-close-window ()
  "kill current buffer and close  its window"
  (interactive)
  (kill-buffer)
  (delete-window))
(bind-key "C-x C-k" #'scame/kill-buffer-and-close-window)

(bind-key "C-x C-j" #'join-line)

;;;###autoload
(defun scame/move-current-file-to-trash ()
  (interactive)
  (when (eq major-mode 'dired-mode)
    (user-error "%s: In dired. Nothing is done." real-this-command))
  (move-file-to-trash buffer-file-name))
(bind-key "C-x x x" #'scame/move-current-file-to-trash)

(bind-key "C-x r a" #'append-to-register)

;;;###autoload
(defun scame/empty-register (register)
  "Clears out value from Emacs register."
  (interactive "cRegister: ")
  (set-register register nil))
(bind-key "C-x r e" #'scame/empty-register)

;;;###autoload
(defun scame/kill-line-backward ()
  "Kill line backwards from the position of the pointer to the beginning of the line."
  (interactive)
  (kill-line 0))
(bind-key "C-S-k" #'scame/kill-line-backward)

(bind-key "M-n" #'flymake-goto-next-error)
(bind-key "M-p" #'flymake-goto-prev-error)

(require 'eglot)
(setq eglot-events-buffer-size 0) ;disable logging and improve perfomance
(define-key eglot-mode-map (kbd "C-c c r") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c c o") #'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "C-c c h") #'eldoc)
(define-key eglot-mode-map (kbd "C-c c a") #'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-c c f") #'eglot-format-buffer)
(define-key eglot-mode-map (kbd "C-c c q") #'eglot-code-action-quickfix)
(define-key eglot-mode-map (kbd "C-c c e") #'eglot-code-action-extract)
(define-key eglot-mode-map (kbd "<f6>") #'xref-find-definitions)
(define-key eglot-mode-map (kbd "M-.") #'xref-find-definitions)
