(require 'use-package)
(setq use-package-always-ensure t)
;; (use-package use-package-ensure-system-package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; prefer GNU packages over NONGNU over Melpa
(customize-set-variable 'package-archive-priorities
			'(("gnu" . 99)
			  ("nongnu" . 98)
			  ("melpa" . 97)))
