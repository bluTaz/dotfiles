;; Bootstrap `use-package'

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

;; Emacs configuration

(setq visible-bell t)
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(savehist-mode 1)

(use-package company
  :ensure t)

;; Appearance

(scroll-bar-mode -1)

(use-package powerline
    :ensure t
    :config
    (powerline-center-evil-theme)

    (use-package flycheck-color-mode-line
        :ensure t
        :config
        (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
    )

(use-package leuven-theme
    :ensure t)

(add-to-list 'default-frame-alist '(font . "Source Code Pro" ))

;; Evil mode

(use-package evil
    :ensure t
    :config
    (evil-mode 1)

    (use-package evil-leader
        :ensure t
        :config
        (global-evil-leader-mode)
        (evil-leader/set-leader "<SPC>")
        (evil-leader/set-key
        "e" 'find-file
        "bb" 'switch-to-buffer
        "bd" 'kill-buffer-and-window
        "by" 'copy-whole-buffer
        "cy" 'clipboard-kill-ring-save
        "cp" 'clipboard-yank
        "fs" 'save-buffer
        "gs" 'magit-status
        "hs" 'split-window-horizontally
        "iu" 'insert-char
        "lf" 'load-file
        "ne" 'flycheck-next-error
        "pe" 'flycheck-previous-error
        "rm" 'notmuch
        "sm" 'message-send-and-exit
        "si" 'whitespace-mode
        "tn" 'linum-mode
        "wl" 'delete-other-windows
        "wk" 'windmove-left
        "wj" 'windmove-right
        "qq" 'save-buffers-kill-emacs
        "zp" 'zeal-at-point
        )
    )

    (use-package evil-surround
        :ensure t
        :config
        (global-evil-surround-mode))

    (use-package evil-indent-textobject
      :ensure t
    )

    (use-package evil-commentary
        :init
        (progn
            (evil-commentary-mode)))
)

;; helm

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    (helm-mode))

;; Major-modes

(use-package markdown-mode
    :ensure t)

(use-package yaml-mode
    :ensure t)

(provide 'init)