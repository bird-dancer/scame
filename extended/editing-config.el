;; -*- lexical-binding: t; -*-
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package pulsar
  :if (display-graphic-p)
  :init
  (pulsar-global-mode t))

(use-package expand-region
  :bind ("C-=" . er/expand-region))
