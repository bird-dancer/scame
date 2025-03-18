(setq completions-sort 'historical)
(savehist-mode t)
(recentf-mode t)

(setq minibuffer-visible-completions t)
(setq completion-auto-wrap t)

(setq read-file-name-completion-ignore-case t)

(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'text-mode-hook #'completion-preview-mode)
(add-hook 'org-mode-hook #'completion-preview-mode)
