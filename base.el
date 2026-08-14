;; -*- lexical-binding: t; -*-
;;; base.el --- simiple configurable agreeable modular emacs  -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(load "base/startup.el" nil t)
(load "base/user-interface.el" nil t)
(load "base/keybindings.el" nil t)
(load "base/completion.el" nil t)
(load "base/org-mode.el" nil t)
(load "base/var.el" nil t)
(load "base/hexl.el" nil t)
(load "base/eshell-settings.el" nil t)
(load "base/dired-settings.el" nil t)
(load "base/editor-config.el" nil t)
(load "base/melpa.el" nil t)
