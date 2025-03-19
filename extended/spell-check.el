;; use on normal systems
(use-package jinx
  :if (not (file-directory-p "~/.guix-profile/share/emacs/site-lisp")) ;only install on non guix system
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; use emacs-jinx package from guix if available
(use-package jinx
  :if (file-directory-p "~/.guix-profile/share/emacs/site-lisp") ;only install on guix system
  :ensure nil
  :load-path "~/.guix-profile/share/emacs/site-lisp/jinx-1.9/"
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; (add-hook 'emacs-startup-hook #'global-jinx-mode)
;; (keymap-global-set "M-$" #'jinx-correct)
;; (keymap-global-set "C-M-$" #'jinx-languages)
;; (keymap-global-set "M-p" #'jinx-previous)
;; (keymap-global-set "M-n" #'jinx-next)
