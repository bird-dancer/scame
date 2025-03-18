(global-set-key (kbd "C-c a") #'org-agenda)
;; make it so the agenda always starts today and we can see two weeks from today instead of just until Sunday
(setq org-agenda-start-day "0d")
(setq org-agenda-span 20)
(setq org-agenda-start-on-weekday nil)

(defun scame-transform-comments (backend)
  (while (re-search-forward "[:blank:]*# " nil t)
    (replace-match "#+LATEX: % ")))
(add-hook 'org-export-before-parsing-hook #'scame-transform-comments)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-F") 'org-shiftmetaright)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-B") 'org-shiftmetaleft)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-P") 'org-move-subtree-up)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-N") 'org-move-subtree-down)))

(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

(custom-set-faces
 '(org-level-1 ((t (:height 1.75))))
 '(org-level-2 ((t (:height 1.5))))
 '(org-level-3 ((t (:height 1.25))))
 '(org-level-4 ((t (:height 1.1))))
 '(org-document-title ((t (:height 1.5)))))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("ba" . "src bash"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("li" . "src lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))
(add-to-list 'org-structure-template-alist '("tex" . "src latex"))
(add-to-list 'org-structure-template-alist '("rs" . "src rust"))

(setq org-confirm-babel-evaluate nil)

(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))
