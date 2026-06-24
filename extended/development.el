(use-package eglot-java
  :defer t)

(use-package auto-virtualenv
  :config
  (setq auto-virtualenv-verbose t)
  (auto-virtualenv-setup))
(use-package indent-bars
  :hook (python-ts-mode . indent-bars-mode))
