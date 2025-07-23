(use-package org-cliplink
  :bind ("C-x p i" . org-cliplink))

(use-package org-download
  ;; :hook ((org-mode . org-download-mode))
  ;; :hook ((dired-mode . org-download-enable))
  :config
  (setq-default org-download-image-dir "./images"))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))
