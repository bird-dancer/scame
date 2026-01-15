(use-package magit
  :bind (("C-x g" . magit))
  :config
  (setq magit-diff-visit-prefer-worktree t))

(use-package diff-hl
  :hook ((text-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)))
