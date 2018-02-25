;; Default Encoding
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; (setq my-file-name
;;       '((:eval (if (buffer-file-name)
;;                    (abbreviate-file-name (buffer-file-name))
;;                  "%b"))))
(setq frame-title-format (list "[%m] %b %*"))

;; Sync clipboards and ignore visual selections
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(fset 'evil-visual-update-x-selection 'ignore)

;; Linum format & padding
(unless (display-graphic-p)
  (setq linum-relative-format "%4s "))

;; Set browser to use for links
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "luakit")

;; Enable adaptive-wrap and visual-line-mode
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)

;; Disable verbose-buffers *Messages* / *Completions*
(setq-default message-log-max nil)

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; Remove *scratch* and *Messages* buffer
(setq initial-scratch-message "")
(defun remove-extra-buffers ()
  ;; (if (get-buffer "*scratch*")
  ;;     (kill-buffer "*scratch*"))
  (if (get-buffer "*Messages*")
      (kill-buffer "*Messages*")))
(add-hook 'after-change-major-mode-hook 'remove-extra-buffers)

;; Ledger Config
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(setq ledger-post-amount-alignment-column 48)
(setq ledger-post-auto-adjust-amounts t)
