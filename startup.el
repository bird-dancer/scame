(setq gc-cons-threshold (* 50 1000 1000))      ;500mb
(setq read-process-output-max (* 2 1024 1024)) ; 2mb

(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold (* 10 1000 1000)))) ;10mb

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")	; make *scratch* buffer blank

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(setq backup-dir (expand-file-name "tmp/backups/" user-emacs-directory))
(setq backup-directory-alist `(("." . , backup-dir)))
(setq delete-old-versions t)

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*", (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
