;; -*- lexical-binding: t; -*-
(add-to-list 'auto-mode-alist '("\\Makefile\\..*" . makefile-gmake-mode))

(use-package eglot
  :defer t
  :ensure nil
  :config
  (eglot-documentation-renderer 'markdown-ts-view-mode)
  (eglot-code-action-indications nil)
  (setq eglot-events-buffer-size 0) ;disable logging and improve perfomance
  (define-key eglot-mode-map (kbd "C-c c r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c c o") #'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c c h") #'eldoc)
  (define-key eglot-mode-map (kbd "C-c c a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c c f") #'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c c q") #'eglot-code-action-quickfix)
  (define-key eglot-mode-map (kbd "C-c c e") #'eglot-code-action-extract)
  (define-key eglot-mode-map (kbd "<f6>") #'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-.") #'xref-find-definitions))

(setq treesit-auto-install-grammar 'always)
(setq treesit-enabled-modes t)
