(use-package magit
  :ensure t)
;; (use-package magit
;; :bind (("C-x g" . magit)
;;        ("C-x c" . magit-clone-shallow)))

(use-package diff-hl
  :hook ((text-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)))
