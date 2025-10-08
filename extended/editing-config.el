(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;;(delete 'rust treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
