;; My emacs init file
(setq gc-cons-threshold 100000000) ;; Set garbage collection threshold

(require 'package) ;; Add the MELPA package repository
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t) ;; org-mode packages
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   '("171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "7922b14d8971cce37ddb5e487dbc18da5444c47f766178e5a4e72f90437c0711" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "efeac8a7737b192859b0683bb97a5c2e4c600101dccda67b78a424fc9a738b75" "4d553fbd7fa02bedfb17e8107680f56e1aa952c073f389c780a5aeaa4896867a" default))
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(line-number-mode nil)
 '(package-selected-packages
   '(lsp use-package with-editor smex ace-jump-mode xwwp osx-clipboard osx-trash pdf-view-restore pdf-tools howdoi nyan-mode go-playground gotest go-errcheck bongo vterm swoop helm-swoop helm-ag god-mode elcord solarized-theme lsp-ui lsp-python-ms lsp-mode flycheck-google-cpplint flycheck-golangci-lint company exec-path-from-shell vue-mode indent-guide neotree go-mode atom-one-dark-theme lua-mode latex-preview-pane auctex fic-mode smooth-scrolling ace-window flycheck))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;=========================================================================
;; Theme and universal editor configurations
;;=========================================================================
(setq byte-compile-warnings '(cl-functions))
;; Set up use-package for auto-installing packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(use-package auto-package-update
  :defer 10
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))

(setq auto-window-vscroll nil)
(set-face-attribute 'default nil :font "Meslo LG L DZ for Powerline" :height 160)

(use-package solarized-theme)
(use-package nyan-mode)
(if window-system
    ;; GUI specific configuration
    (progn
      (load-theme 'solarized-dark t)
      (nyan-mode 1)
      (server-start))
  ;; Terminal specific configuration
  (progn
    (add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
    (load-theme 'solarized t)
    (set-terminal-parameter nil 'background-mode 'dark)))

(when (version<= "26.0.50" emacs-version ) ;; Display line numbers
  (global-display-line-numbers-mode))
;; Add indent guides
 (indent-guide-global-mode)

(electric-pair-mode 1) ;; Enable electric-pair-mode
(show-paren-mode) ;; Highlight matching parentheses

;; (require 'elcord)
;; (elcord-mode) ;; Discord rich presence

;;(use-package powerline) ;; Enable powerline
;;(powerline-vim-theme)

(use-package vterm
  :config (global-set-key (kbd "C-c t") #'vterm))

(use-package pdf-tools)
(use-package pdf-view-restore
  :hook (pdf-view-mode . pdf-view-restore-mode))

(use-package smooth-scrolling)
(smooth-scrolling-mode 1)

(use-package god-mode
  :bind (("C-c g" . god-mode-all)))

(use-package ace-window
  :bind (("M-o" . ace-window)))
(global-set-key (kbd "C-c z") #'zap-up-to-char)
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)

(use-package neotree
  :config (global-set-key (kbd "C-c n") #'neotree)
  :custom (neo-smart-open t))
(global-set-key (kbd "C-c k") #'comment-or-uncomment-region)

(use-package ace-jump-mode
  :config (global-set-key (kbd "C-c SPC") #'ace-jump-mode))

(use-package helm
  :config
  (global-set-key (kbd "C-c h") #'helm-browse-project)
  (global-set-key (kbd "C-x b") #'helm-buffers-list))

(use-package smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)) ;; Old M-x.

(use-package osx-clipboard
  :config (osx-clipboard-mode 1))
(use-package osx-trash
  :config (osx-trash-setup)
  :custom (delete-by-moving-to-trash 1))

(use-package xwwp
  :custom (xwwp-search-prefix "https://duckduckgo.com/?q="))
(defun dired-webkit-open ()
  "Open a file in xwidgets webkit from dired mode"
  (interactive)
  (dired-copy-filename-as-kill 0)
  (xwidget-webkit-browse-url (concat "file://" (current-kill 0)))
  (let ((b (get-buffer "*xwidget-webkit*")))
    (if b (switch-to-buffer b))))
(add-hook 'dired-mode-hook (lambda ()
                             (local-set-key (kbd "C-c o") 'dired-webkit-open)
                             ))

(put 'dired-find-alternate-file 'disabled nil)
(toggle-frame-fullscreen) ;; Start in full-screen
;;=========================================================================
;; Coding
;;=========================================================================
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(setq-default indent-tabs-mode nil) ;; Don't use tabs to indent
(setq c-basic-offset 4)

(use-package indent-guide)
;; scrub-mode is a macro for several modes to make coding easier
(use-package flycheck)
(use-package company)
(use-package fic-mode)
(use-package lsp-mode
  :custom
  (lsp-prefer-flymake nil))
(use-package lsp-ui)
(define-minor-mode scrub-mode "Coding for zoomers.")
(add-hook 'scrub-mode-hook 'flycheck-mode)
(add-hook 'scrub-mode-hook 'company-mode)
(add-hook 'scrub-mode-hook 'fic-mode)
(add-hook 'scrub-mode-hook #'lsp)

;; Company mode auto completion settings
(use-package company
  :custom
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t))

(add-hook 'prog-mode-hook #'recentf-mode)
(add-hook 'prog-mode-hook (lambda () (setq fill-column 100)))
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ;; Get line ruler column
(add-hook 'prog-mode-hook 'fic-mode)
(defun my-go-mode-hook () ;; Custom go hooks
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  )
(use-package go-mode
  :config
  (add-hook 'go-mode-hook (lambda () (setq-default tab-width 4))) ;; Set tab width to 4 for Go mode
  (add-hook 'go-mode-hook 'my-go-mode-hook))


(add-hook 'text-mode-hook 'flyspell-mode) ;; Add spell check to text mode
(add-hook 'text-mode-hook 'visual-line-mode) ;; Add line wrap to text mode
(add-hook 'text-mode-hook (lambda () (setq fill-column 200)))

(use-package with-editor
  :config
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)
  (add-hook 'vterm-mode-hook  'with-editor-export-editor))


;;=========================================================================
;; Misc
;;=========================================================================
;; Some LaTeX stuff
(use-package flyspell)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook (lambda () (setq fill-column 200)))

;; Artist mode shortcuts
(add-hook 'artist-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<f15>") 'org-mode)
	    (local-set-key (kbd "<f16>") 'artist-select-op-pen-line) ; f16 = pen mode
            (local-set-key (kbd "<f17>") 'artist-select-op-line)     ; f17 = line
	    (local-set-key (kbd "<f18>") 'artist-select-op-square)   ; f18 = rectangle
	    (local-set-key (kbd "<f19>") 'artist-select-op-ellipse)  ; f19 = ellipse
	    (local-set-key (kbd "C-z") 'undo)))

;; org-mode keybindings
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-l") 'org-store-link)
            (local-set-key (kbd "C-a") 'org-agenda)
            (local-set-key (kbd "C-c") 'org-capture)
            (local-set-key (kbd "C-b") 'org-switchb)))

(provide '.emacs)
;;; .emacs ends here
