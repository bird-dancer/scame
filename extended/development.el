;; -*- lexical-binding: t; -*-
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package nix-ts-mode
  :mode ("\\.nix\\'" . nix-ts-mode))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (setq rust-mode-treesitter-derive t))

(use-package gleam-ts-mode)

(use-package eglot-java
  :defer t)

(use-package auto-virtualenv
  :config
  (setq auto-virtualenv-verbose t)
  (auto-virtualenv-setup))
(use-package indent-bars
  :hook (python-ts-mode . indent-bars-mode))
