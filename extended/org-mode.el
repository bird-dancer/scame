(use-package org-cliplink
  :after org
  :bind ("C-x p i" . org-cliplink))

(use-package org-download
  :after org
  ;; :hook ((org-mode . org-download-mode))
  ;; :hook ((dired-mode . org-download-enable))
  :config
  (setq-default org-download-image-dir "./images"))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package ox-latex
  :ensure nil
  :after org
  :config
  (add-to-list 'org-cite-export-processors '(html csl))
  (add-to-list 'org-cite-export-processors '(latex biblatex))
  ;; turn off font locking for citations so it doesnt lag. can be removed when using citar
  (setq org-cite-activate-processor nil))

(use-package engrave-faces
  :after org
  :config
  (setq org-latex-src-block-backend 'engraved)
  (add-to-list 'org-latex-packages-alist '("" "float")))
