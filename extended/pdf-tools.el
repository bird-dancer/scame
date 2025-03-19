(use-package pdf-tools
  :if (not (file-directory-p "~/.guix-profile/share/emacs/site-lisp")) ;only install on non guix system
  :mode ("\\.pdf\\'" . pdf-view-mode))
(use-package pdf-tools
  :if (file-directory-p "~/.guix-profile/share/emacs/site-lisp") ;only install on guix system
  :ensure nil
  :load-path "~/.guix-profile/share/emacs/site-lisp/pdf-tools-1.1.0"
  :mode ("\\.pdf\\'" . pdf-view-mode))
