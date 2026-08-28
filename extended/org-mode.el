;; -*- lexical-binding: t; -*-
(use-package org-cliplink
  :after org
  :bind ("C-x p i" . org-cliplink))

(use-package org-download
  :hook ((org-mode . org-download-enable)
	 (dired-mode . org-download-enable))
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
