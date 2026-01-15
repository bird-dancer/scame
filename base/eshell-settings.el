;; don't scroll to bottom on clear
(use-package em-alias
  :defer t
  :config
  (add-to-list 'eshell-command-aliases-list (list "clear" "clear 1")))
