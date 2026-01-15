(use-package restclient
  :defer t)
(use-package jq-mode
  :defer t)
(use-package restclient-jq
  :defer t)

(use-package pdf-tools
  :if (display-graphic-p)
  :if (not (file-directory-p "~/.guix-profile/share/emacs/site-lisp"))
  :mode ("\\.pdf\\'" . pdf-view-mode))
;; (use-package pdf-tools
;;   :if (file-directory-p "~/.guix-profile/share/emacs/site-lisp") ;only install on guix system
;;   :ensure nil
;;   :load-path "~/.guix-profile/share/emacs/site-lisp/pdf-tools-1.1.0"
;;   :mode ("\\.pdf\\'" . pdf-view-mode))
