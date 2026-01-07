;; don't scroll to bottom on clear
(require 'em-alias)
(add-to-list 'eshell-command-aliases-list (list "clear" "clear 1"))
