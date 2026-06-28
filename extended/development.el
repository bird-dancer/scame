(use-package nix-ts-mode
  :mode ("\\.nix\\'" . nix-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
    	       '(erlang-ts-mode . ("elp" "server"))))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
     	       '(erlang-mode . ("elp" "server"))))

(use-package eglot-java
  :defer t)

(use-package auto-virtualenv
  :config
  (setq auto-virtualenv-verbose t)
  (auto-virtualenv-setup))
(use-package indent-bars
  :hook (python-ts-mode . indent-bars-mode))
