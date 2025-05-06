(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-du
  :after dired
  :config (setq dired-du-size-format t))
