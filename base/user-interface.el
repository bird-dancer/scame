(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(global-visual-line-mode 1)
;; (setq visual-wrap-prefix "⇢ ") ;; choose a visible symbol
;; (global-visual-wrap-prefix-mode 1)

(global-visual-line-mode t)
(setq-default cursor-type 'bar)	 ;use bar as cursor
(setq visible-bell t)		 ;flash screen instead of ringing bell
(global-hl-line-mode t)			;highlight current line
(setq project-mode-line t)		;show current project name is mode line
(setq use-dialog-box nil)		;disable UI dialogs as prompts
(global-prettify-symbols-mode t)
(pixel-scroll-precision-mode)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
