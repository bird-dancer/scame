(use-package restclient
  :defer t)
(use-package jq-mode
  :defer t)
(use-package restclient-jq
  :defer t)

(use-package ghostel
  :bind(("C-c c c" . ghostel-compile)
      	("C-c C-c c" . ghostel-compile)
      	("C-c C-c r" . ghostel-recompile)
      	("C-c C-c d" . ghostel-compile-debug)
	("C-M-<return>" . ghostel))
  :config
  (setq ghostel-progress-function #'ghostel-spinner-progress)
  ;; (setq ghostel-progress-function #'ghostel-default-progress)
  (setq ghostel-spinner-type 'horizontal-moving)
  ;; make cursor bar
  (setq ghostel-ignore-cursor-change t)
  (ghostel--set-cursor-style 0 t))

(use-package pdf-tools
  :if (display-graphic-p)
  :if (not (file-directory-p "~/.guix-profile/share/emacs/site-lisp"))
  :mode ("\\.pdf\\'" . pdf-view-mode))
;; (use-package pdf-tools
;;   :if (file-directory-p "~/.guix-profile/share/emacs/site-lisp") ;only install on guix system
;;   :ensure nil
;;   :load-path "~/.guix-profile/share/emacs/site-lisp/pdf-tools-1.1.0"
;;   :mode ("\\.pdf\\'" . pdf-view-mode))
