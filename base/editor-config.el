(add-hook 'prog-mode-hook #'hs-minor-mode)
(bind-key "C-c C-h" #'hs-hide-block)
(bind-key "C-c C-s" #'hs-show-block)
(bind-key "C-c C-t" #'hs-toggle-hiding)
(bind-key "C-<tab>" #'hs-toggle-hiding)
(bind-key "C-c C-a" #'hs-show-all)
(bind-key "C-c C-l" #'hs-hide-all)

;; (add-hook 'prog-mode-hook #'electric-pair-mode)

(electric-indent-mode t)
(electric-quote-mode t)
					;(setq minibuffer-default-prompt-format " [%s]")
(minibuffer-electric-default-mode 1)
