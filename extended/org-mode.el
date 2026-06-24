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

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)
;; org-src-preserve-indentation t)

(with-eval-after-load 'org
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(setq org-latex-src-block-backend 'minted)
(with-eval-after-load 'org
  (add-to-list 'org-latex-packages-alist '("" "minted")))

(setq org-latex-minted-options
      '(("breaklines" "true")          ;; Enables line wrapping
        ;; ("breakanywhere" "true")       ;; Allows breaking in the middle of long strings/hex values
        ;; ("style" "tango")              ;; A clean, readable color theme (or try "colorful", "monokai")
        ;; ("fontsize" "\\footnotesize")  ;; Slightly smaller font to fit more code
	("xleftmargin" "-35pt")  ;; Pushes the left border outward (or use negative like "-10pt" to pull it into the margin)
	("xrightmargin" "-35pt") ;; Pushes the right border outward
        ("frame" "single")             ;; Adds a neat box around the code block
	;; ("linenos" "true")
	;; ("numberstyle" "\\normalsize\\ttfamily")
	;; ("numbersep" "10pt")
	))
;; Optional: Adds line numbers
