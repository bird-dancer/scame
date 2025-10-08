(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 10) ;; Show more candidates
  ;;(vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Option 1: Additional bindings
(keymap-set vertico-map "?" #'minibuffer-completion-help)
;; (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
(keymap-set vertico-map "M-TAB" #'minibuffer-complete)

(keymap-set vertico-map "M-RET" #'minibuffer-complete-and-exit)
(keymap-set vertico-map "C-j" #'minibuffer-complete-and-exit)
(keymap-set vertico-map "C-TAB" #'minibuffer-complete)

;; Option 2: Replace `vertico-insert' to enable TAB prefix expansion.
;; (keymap-set vertico-map "TAB" #'minibuffer-complete)

(use-package orderless
  ;; :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)

  ;;     (completion-styles '(orderless basic))
  ;; (completion-category-defaults nil)
  ;; (completion-category-overrides '((file (styles partial-completion))))
  :config
  (setq completion-styles '(orderless flex)
        completion-category-overrides '((eglot (styles . (orderless flex))))))

(use-package marginalia
  :after vertico
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package consult
  :bind (("C-c r" . consult-ripgrep)
         ("C-c s" . consult-line))
  :config
  (keymap-set minibuffer-local-map "C-r" 'consult-history)
  (setq completion-in-region-function #'consult-completion-in-region))

(defun consult-ripgrep-current-dir (&optional dir)
  "Search using consult-ripgrep in the specified DIR or current directory."
  (interactive "P")
  (consult-ripgrep (or dir default-directory)))

(bind-key "C-c M-r" #'consult-ripgrep-current-dir)
