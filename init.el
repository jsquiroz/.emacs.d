;; Do not show the startup screen.
(setq inhibit-startup-message t)

;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)


;; Highlight current line.
(global-hl-line-mode t)


;; autoclose
(electric-pair-mode 1)

;; Disabled backup files
(setq make-backup-files nil)

;; Show line number
(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)

;; set font
(set-frame-font "Monaco" nil t)
(set-face-attribute 'default nil :height 150)
;;
;; Package managment
;;
(require 'package)
(package-initialize)

;;
;; ido mode
;;
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; (ido-mode 'buffers)

;; (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
;;	       "*Messages*" "Async Shell Command" "KILL"))
;; (setq ido-file-extensions-order '(".org" ".tex" ".emacs" ".pdf" ".txt"))

;; Add `melpa` to `package-archives`.
(add-to-list 'package-archives
             '("melpa" . "https://stable.melpa.org/packages/") t)

(add-to-list 'load-path "~/.emacs.d/lisp/")


(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))


;;; OS X support
(use-package ns-win                     ; OS X window support
  :defer t
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the
                                        ; workspace
        mac-option-modifier 'meta       ; Option is simply the natural Meta
        mac-command-modifier 'meta      ; But command is a lot easier to hit
        mac-right-command-modifier 'left
        mac-right-option-modifier 'none ; Keep right option for accented input
        ;; Just in case we ever need these keys
        mac-function-modifier 'hyper))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))


(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; El antiguo comportamiento de M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package flycheck
  :ensure t)

(use-package go-mode
  :config
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  (flycheck-mode)
  (add-hook 'go-mode-hook 'company-mode)
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'flycheck-mode)
  (add-hook 'go-mode-hook '(lambda ()
                 (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
  (add-hook 'go-mode-hook '(lambda ()
                 (local-set-key (kbd "C-c C-g") 'go-goto-imports)))
  (add-hook 'go-mode-hook (lambda ()
                (set (make-local-variable 'company-backends) '(company-go))
                (company-mode))))

;; Theme nord
;;(use-package nord-theme
;;  :ensure t
;;  :config
;;  (load-theme 'nord t)
;;)

;;(use-package vscode-dark-plus-theme
;;  :ensure t
;;  :config
;;  (load-theme 'vscode-dark-plus t))


(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))


;;(setq sml/no-confirm-load-theme t)
;;(setq sml/theme 'respectful)
;;(sml/setup)
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))


(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))


(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(require 'prettier-js)
  (setq prettier-js-args '(
                           "--trailing-comma" "es5"
                           "--single-quote" "true"
                           "--print-width" "120"
                           "--tab-width" "4"
                           "--use-tabs" "false"
                           "--jsx-bracket-same-line" "false"
                           "--stylelint-integration" "true"
                           ))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))


(use-package js2-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :mode(("\\.js\\'" . rjsx-mode)
        ("\\.jsx\\'" . rjsx-mode))
  :init
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'tide-mode))

(use-package tide
  :ensure t
  :mode(("\\.ts\\'" . typescript-mode))
  :init
  (add-hook 'typescript-mode-hook 'tide-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  :config
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package json-mode
  :ensure t)


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(package-selected-packages
   '(json-mode rjsx-mode smartparens prettier-js which-key helm-projectile projectile helm ido-vertical-mode smex sml smart-mode-line powerline magit dracula-theme all-the-icons neotree dired-sidebar tide js2-mode vscode-dark-plus-theme nord-theme exec-path-from-shell flycheck yasnippet use-package lsp-ui go-mode company-lsp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
