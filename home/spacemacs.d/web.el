(defun web-mode-flyspefll-verify ()
  (let ((f (get-text-property (- (point) 1) 'face)))
    (not (memq f '(web-mode-html-attr-value-face
                   web-mode-html-tag-face
                   web-mode-html-attr-name-face
                   web-mode-doctype-face
                   web-mode-keyword-face
                   web-mode-function-name-face
                   web-mode-variable-name-face
                   web-mode-css-property-name-face
                   web-mode-css-selector-face
                   web-mode-css-color-face
                   web-mode-type-face
                   )
               ))))
(put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspefll-verify)

(add-hook 'web-mode-hook
          (lambda ()
            (emmet-mode 1)
            (flyspell-mode 1)
            (visual-line-mode 1)
            (setq emmet-self-closing-tag-style "")
            (define-key emmet-mode-keymap (kbd "TAB") 'emmet-expand-line)
            (define-key emmet-mode-keymap (kbd "<tab>") 'emmet-expand-line)
            (setq emmet-move-cursor-between-quotes t)

            ;; HTML Validation using html5check.py
            (local-set-key (kbd "C-c v")
                           (lambda () (interactive)
                             (save-buffer)
                             (shell-command (concat "html5check.py " (shell-quote-argument (buffer-file-name))))
          ))
))

(eval-after-load 'flycheck
  '(flycheck-add-mode 'html-tidy 'web-mode))
