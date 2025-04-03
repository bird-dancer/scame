(use-package multiple-cursors
  :bind (("C-;" . mc/edit-lines)	;non standard
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :init
  (setq  mc/match-cursor-style nil))	;or else cursors dont show up when style is bar
