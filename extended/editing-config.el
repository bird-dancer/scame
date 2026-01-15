(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package pulsar
  :if (display-graphic-p)
  :config
  (pulsar-global-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; (delete 'rust treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
