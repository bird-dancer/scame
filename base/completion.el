(setq completions-sort 'historical)
;;(setq history-length 50)
(savehist-mode t)
(recentf-mode t)

(setq minibuffer-visible-completions t)
(setq completion-auto-wrap t)

(setq read-buffer-completion-ignore-case t) ;when switching buffers
(setq read-file-name-completion-ignore-case t) ;when finding file

(global-completion-preview-mode t)
