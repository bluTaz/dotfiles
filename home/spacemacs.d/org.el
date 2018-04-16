(global-visual-line-mode 1)

(require 'org)

;; (with-eval-after-load 'org-agenda
;;   (require 'org-projectile)
;;   (push (org-projectile:todo-files) org-agenda-files)
;;   )

(setq org-src-fontify-natively t
      org-hide-emphasis-markers t
      org-enforce-todo-checkbox-dependencies t
      org-enforce-todo-dependencies t
      org-tab-follows-link t
      evil-org-key-theme '(textobjects navigation additional insert todo)
  )

(setq-default
 org-babel-load-languages '((ledger . t)
  ))
