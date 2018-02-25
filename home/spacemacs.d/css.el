(add-hook 'css-mode-hook 'rainbow-mode)

;; (add-hook 'css-mode-hook
;;           '(lambda ()
;;              (add-hook 'after-save-hook
;;                        (lambda () (when (executable-find "csstidy") (css-check))) nil t)
;;              (rainbow-mode 1)))
