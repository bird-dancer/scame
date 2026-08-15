;; -*- lexical-binding: t; -*-
(require 'package)
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)
(setq package-install-upgrade-built-in t)
