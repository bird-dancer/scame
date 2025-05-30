(use-package org-cliplink
  :bind ("C-x p i" . org-cliplink))

(use-package org-download
  :hook ((dired-mode . org-download-enable)))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))
