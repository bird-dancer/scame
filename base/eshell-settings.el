(setq explicit-shell-file-name (getenv "SHELL"))
(setq shell-command-switch "-ic")

;; don't scroll to bottom on clear
(require 'em-alias)
(add-to-list 'eshell-command-aliases-list (list "clear" "clear 1"))

(add-hook 'eshell-mode-hook
	  (lambda () (setenv "TERM" "xterm-256color" t)))

(defun scame/eshell-load-bash-aliases ()
  (interactive)
  "Read Bash aliases and add them to the list of eshell aliases,
ensuring all arguments are passed through."
  ;; Bash needs to be run - temporarily - interactively in order to get the list of aliases.
  (with-temp-buffer
    (call-process "bash" nil '(t nil) nil "-ci" "alias")
    (goto-char (point-min))
    (while (re-search-forward "alias \\(.+\\)='\\(.+\\)'$" nil t)
      (eshell/alias (match-string 1) (concat (match-string 2) " $*")))))
;; (add-hook 'eshell-alias-load-hook 'eshell-load-bash-aliases)
