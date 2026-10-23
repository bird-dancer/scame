;; -*- lexical-binding: t; -*-
(add-to-list 'auto-mode-alist '("\\Makefile\\..*" . makefile-gmake-mode))

(use-package eglot
  :defer t
  :ensure nil
  :init
  (fset #'jsonrpc--log-event #'ignore)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 '((ruby-mode ruby-ts-mode) "ruby-lsp")))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 '((erlang-mode erlang-ts-mode) . ("elp" "server"))))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format full))
  ;; (eglot-events-buffer-size 0) ;disable logging and improve perfomance
  (eglot-prefer-plaintext nil)
  (jsonrpc-event-hook nil)
  (eglot-code-action-indications nil)
  (eglot-documentation-renderer 'markdown-ts-view-mode)
  :bind (:map
	 eglot-mode-map
	 ("C-c c r" . eglot-rename)
	 ("C-c c o" . eglot-code-action-organize-imports)
	 ("C-c c h" . eldoc)
	 ("C-c c a" . eglot-code-actions)
	 ("C-c c f" . eglot-format-buffer)
	 ("C-c c q" . eglot-code-action-quickfix)
	 ("C-c c e" . eglot-code-action-extract)
	 ("C-c c i" . eglot-inlay-hints-mode)
	 ("<f6>" . xref-find-definitions)
	 ("M-." . xref-find-definitions)))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

(setq treesit-auto-install-grammar 'always)
(setq treesit-enabled-modes t)
