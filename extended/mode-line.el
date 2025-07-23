(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t)
  :config
  (display-battery-mode)
  (setq display-time-24hr-format t)
  (display-time))
