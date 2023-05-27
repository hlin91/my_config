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
 '(catppuccin-flavor 'frappe)
 '(custom-safe-themes
   '("8d69d4efbc29aa7ad70fc162447576c0327262c3e6baa8d7a41c90b981cc93f9" "276c08753eae8e13d7c4f851432b627af58597f2d57b09f790cb782f6f043966" "bb6d3df2670c74f4ad513512feea89cdbe3040c1d0ba00cf641d63679ee3d0fc" "5963c22b5f105090a406f6a9eff7e61aa4c64b564cabf195f3021e4752b35132" "fbd91b2e6dc5c7912e86406226638adb014612386516e078a0447195591447d3" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "ba72dfc6bb260a9d8609136b9166e04ad0292b9760a3e2431cf0cd0679f83c3a" "41098e2f8fa67dc51bbe89cce4fb7109f53a164e3a92356964c72f76d068587e" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "b186688fbec5e00ee8683b9f2588523abdf2db40562839b2c5458fcfb322c8a4" "aba75724c5d4d0ec0de949694bce5ce6416c132bb031d4e7ac1c4f2dbdd3d580" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "7922b14d8971cce37ddb5e487dbc18da5444c47f766178e5a4e72f90437c0711" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "efeac8a7737b192859b0683bb97a5c2e4c600101dccda67b78a424fc9a738b75" "4d553fbd7fa02bedfb17e8107680f56e1aa952c073f389c780a5aeaa4896867a" default))
 '(eglot-confirm-server-initiated-edits nil)
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(line-number-mode nil)
 '(package-selected-packages
   '(esup helm-c-yasnippet yasnippet-snippets yasnippet helm-grepint consult-eglot consult-flycheck embark-consult consult-embark embark consult catppuccin-theme powerline all-the-icons writegood-mode minimap hydra eglot multiple-cursors ewal-doom-themes ewal-spacemacs-themes helm emacs-async gdscript-mode ewal rainbow-mode git-gutter qml-mode projectile leuven-theme doom-themes rust-mode rainbow-delimiters ace-window tree-sitter-langs tree-sitter lsp-mode lsp-ui lsp use-package with-editor smex ace-jump-mode osx-clipboard osx-trash howdoi nyan-mode go-playground gotest go-errcheck bongo vterm swoop helm-swoop helm-ag elcord lsp-python-ms flycheck-google-cpplint flycheck-golangci-lint company exec-path-from-shell indent-guide neotree go-mode atom-one-dark-theme lua-mode latex-preview-pane auctex fic-mode smooth-scrolling flycheck))
 '(powerline-default-separator 'rounded)
 '(powerline-gui-use-vcs-glyph t)
 '(select-enable-clipboard t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(warning-suppress-types
   '((use-package)
     (lsp-mode)
     (lsp-mode)
     (lsp-mode)
     (lsp-mode)
     (comp)
     (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-blue ((t (:background "cornflower blue" :foreground "cornflower blue"))))
 '(ansi-color-bright-blue ((t (:background "dodger blue" :foreground "dodger blue"))))
 '(ansi-color-bright-cyan ((t (:background "light sky blue" :foreground "light sky blue"))))
 '(ansi-color-bright-green ((t (:background "PaleGreen1" :foreground "PaleGreen1"))))
 '(ansi-color-bright-magenta ((t (:background "plum" :foreground "plum"))))
 '(ansi-color-bright-red ((t (:background "IndianRed1" :foreground "IndianRed1"))))
 '(ansi-color-bright-yellow ((t (:background "khaki1" :foreground "khaki1"))))
 '(ansi-color-cyan ((t (:background "CadetBlue1" :foreground "CadetBlue1"))))
 '(ansi-color-green ((t (:background "pale green" :foreground "pale green"))))
 '(ansi-color-magenta ((t (:background "orchid" :foreground "orchid"))))
 '(ansi-color-red ((t (:background "IndianRed3" :foreground "IndianRed3"))))
 '(ansi-color-yellow ((t (:background "khaki2" :foreground "khaki2"))))
 '(company-preview ((t (:background "#2f3244" :foreground "plum"))))
 '(ffap ((t (:foreground "plum"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "plum"))))
 '(helm-buffer-not-saved ((t (:extend t :foreground "plum"))))
 '(helm-buffer-process ((t (:extend t :foreground "plum1"))))
 '(helm-buffer-saved-out ((t (:extend t :foreground "plum1"))))
 '(helm-buffer-size ((t (:extend t :foreground "plum"))))
 '(helm-candidate-number ((t (:extend t :foreground "plum"))))
 '(helm-delete-async-message ((t (:extend t :foreground "khaki"))))
 '(helm-ff-denied ((t (:extend t :background "gray20" :foreground "OrangeRed3"))))
 '(helm-ff-directory ((t (:extend t :foreground "plum"))))
 '(helm-ff-dotted-directory ((t (:extend t :foreground "light steel blue"))))
 '(helm-ff-executable ((t (:extend t :foreground "pale green"))))
 '(helm-ff-file ((t (:extend t :foreground "SkyBlue1"))))
 '(helm-ff-file-extension ((t (:extend t :foreground "plum"))))
 '(helm-ff-invalid-symlink ((t (:extend t :foreground "medium orchid"))))
 '(helm-ff-prefix ((t (:extend t :foreground "plum"))))
 '(helm-ff-symlink ((t (:extend t :foreground "plum"))))
 '(helm-grep-cmd-line ((t (:extend t :foreground "plum"))))
 '(helm-grep-file ((t (:extend t :foreground "SkyBlue1"))))
 '(helm-grep-finish ((t (:extend t :foreground "pale green"))))
 '(helm-grep-lineno ((t (:extend t :foreground "plum"))))
 '(helm-header ((t (:extend t :foreground "plum"))))
 '(helm-moccur-buffer ((t (:extend t :foreground "plum"))))
 '(helm-separator ((t (:extend t :foreground "plum"))))
 '(helm-source-header ((t (:extend t :foreground "plum"))))
 '(helm-visible-mark ((t (:extend t :foreground "plum"))))
 '(powerline-active1 ((t nil))))

;;=========================================================================
;; Theme and universal editor configurations
;;=========================================================================

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq byte-compile-warnings '(cl-functions))

(defun delete-inside-char (char)
  (interactive (list (read-char-from-minibuffer "Delete inside char: "
                                                nil 'read-char-history)))
  (search-backward (string char))
  (forward-char)
  (zap-up-to-char 1 (cond
        ((equal char ?\[) ?\])
        ((equal char ?\() ?\))
        ((equal char ?{) ?})
        (t char))))

(global-set-key (kbd "C-c C-z") #'zap-up-to-char)
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "C-c d") #'delete-inside-char)

;; Set up use-package for auto-installing packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(setq use-package-always-ensure t)
(use-package async)
(use-package auto-package-update
  :defer 100
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))

(setq auto-window-vscroll nil)
(set-face-attribute 'default nil :height 160)

(use-package doom-themes
  :defer t)
(use-package catppuccin-theme
  :defer t)

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if (display-graphic-p (selected-frame))
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 240))
           (add-to-list 'default-frame-alist (cons 'width 100)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 400)
                          (frame-char-height)))))))

(defun set-up-window ()
  ;; Configurations specific to windowed mode
  (load-theme 'catppuccin)
  (if (display-graphic-p (selected-frame))
      (progn
        (setq x-select-enable-clipboard 1)
        (set-frame-size-according-to-resolution)
        (set-frame-font "comic code ligatures:size=18"))
    (progn
      (set-face-background 'default "unspecified-bg" (selected-frame))
      (xterm-mouse-mode 1))))

(add-hook 'window-setup-hook 'set-up-window)
(add-hook 'server-after-make-frame-hook 'set-up-window)

(use-package minimap
  :if (display-graphic-p)
  :autoload minimap-mode
  :init
  (setq minimap-width-fraction 0.01)
  (setq minimap-window-location 'right)
  (setq minimap-minimum-width 10))

(use-package powerline
  :if (display-graphic-p))

(when (version<= "26.0.50" emacs-version ) ;; Display line numbers
  (global-display-line-numbers-mode))
;; Add indent guides
(use-package indent-guide
  :config
  (indent-guide-global-mode 1))


(electric-pair-mode 1) ;; Enable electric-pair-mode
(show-paren-mode) ;; Highlight matching parentheses

;; (use-package elcord)
;; (elcord-mode) ;; Discord rich presence

(use-package vterm
  :bind
  (("C-c C-t" . vterm)))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(use-package ace-window
  :bind(("M-o" . ace-window)))

(use-package all-the-icons
  :defer t
  :if (display-graphic-p))
(use-package neotree
  :bind
  (("C-c C-n" . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :custom
  (neo-smart-open t))

(global-set-key (kbd "C-c C-k") #'comment-or-uncomment-region)

(use-package avy
  :bind
  (("C-c C-SPC" . avy-goto-char)
   ("C-c M-SPC" . avy-goto-char-2)
   ("M-g M-f" . avy-goto-line)
   ("M-g M-w" . avy-goto-word-1)
   ("M-g M-e" . avy-goto-word-0)))

(use-package helm
  :defer t
  :init
  (bind-key "C-c C-h" 'helm-browse-project)
  (bind-key "C-x b" 'helm-buffers-list)
  (bind-key "C-x C-f" 'helm-find-files))
(use-package helm-grepint
  :defer t
  :init
  (bind-key "C-c g" 'helm-grepint-grep)
  (bind-key "C-c G" 'helm-grepint-grep-root)
  :config
  (helm-grepint-set-default-config-latest))

(use-package smex
  :init
  (bind-key "M-x" 'smex)
  (bind-key "M-X" 'smex-major-mode-commands)
  (bind-key "C-c C-c M-x" 'execute-extended-command)
  :config
  (smex-initialize)) ;; Old M-x.

;; Get the backslash key back on JP keyboards
;; (global-set-key (kbd "M-¥") (lambda ()
;;                               (interactive)
;;                               (insert-char #x5c)
;;                               ))

;; (use-package xwwp
;;   :custom (xwwp-search-prefix "https://duckduckgo.com/?q="))
;; (defun dired-webkit-open ()
;;   "Open a file in xwidgets webkit from dired mode"
;;   (interactive)
;;   (dired-copy-filename-as-kill 0)
;;   (xwidget-webkit-browse-url (concat "file://" (current-kill 0)))
;;   (let ((b (get-buffer "*xwidget-webkit*")))
;;     (if b (switch-to-buffer b))))
;; (add-hook 'dired-mode-hook (lambda ()
;;                              (local-set-key (kbd "C-c o") 'dired-webkit-open)
;;                              ))

(use-package rainbow-delimiters)

(use-package embark
  :bind
  (("M-RET" . embark-act)))

(use-package consult
  :defer t)
(use-package consult-flycheck
  :defer t)
(use-package consult-eglot
  :defer t)
(use-package embark-consult
  :defer t)

(put 'dired-find-alternate-file 'disabled nil)

(global-auto-revert-mode t)

;;=========================================================================
;; Coding
;;=========================================================================
(use-package yasnippet
  :autoload
  ((yas-global-mode)
   (yas-minor-mode)))
(use-package yasnippet-snippets
  :autoload
  ((yas-global-mode)
   (yas-minor-mode)))
(use-package helm-c-yasnippet
  :bind
  (("C-c y" . helm-yas-complete)))

(use-package multiple-cursors
  :defer t
  :config
  (global-set-key (kbd "C-M-l") 'mc/edit-lines)
  (global-unset-key (kbd "C-M-<mouse-1>"))
  (global-set-key (kbd "C-M-<mouse-1>") 'mc/add-cursor-on-click))

(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

(use-package blamer
  :bind
  (("C-c b" . blamer-show-commit-info)))

(use-package projectile
  :defer t)

(use-package exec-path-from-shell
  :defer t
  :config (exec-path-from-shell-initialize))

(setq-default indent-tabs-mode nil) ;; Don't use tabs to indent
(setq c-basic-offset 4)

(use-package indent-guide)
;; Company mode auto completion settings
(use-package company
  :autoload company-mode
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t))
(use-package flycheck
  :autoload flycheck-mode)
(use-package fic-mode
  :autoload fic-mode)
(use-package eglot
  :autoload eglot)
(use-package tree-sitter
  :autoload tree-sitter-mode)
(use-package tree-sitter-langs
  :autoload tree-sitter-mode
  :requires tree-sitter)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;; scrub-mode is a macro for several modes to make coding easier
(define-minor-mode scrub-mode "Coding for zoomers.")
(add-hook 'scrub-mode-hook 'flycheck-mode)
(add-hook 'scrub-mode-hook 'company-mode)
(add-hook 'scrub-mode-hook 'tree-sitter-mode)
(add-hook 'prog-mode-hook 'yas-global-mode)

(add-hook 'prog-mode-hook #'recentf-mode)
(add-hook 'prog-mode-hook (lambda () (setq fill-column 140)))
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ;; Get line ruler column
(add-hook 'prog-mode-hook 'fic-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

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
  :autoload go-mode
  :config
  (add-hook 'go-mode-hook (lambda () (setq-default tab-width 4))) ;; Set tab width to 4 for Go mode
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package rust-mode
  :autoload rust-mode
  :config
  (add-hook 'rust-mode-hook 'rust-enable-format-on-save))

(add-hook 'text-mode-hook 'flyspell-mode) ;; Add spell check to text mode
(add-hook 'text-mode-hook 'visual-line-mode) ;; Add line wrap to text mode
(add-hook 'text-mode-hook 'writegood-mode)
(add-hook 'text-mode-hook 'flycheck-mode)
(add-hook 'text-mode-hook (lambda () (setq fill-column 200)))

;; (use-package with-editor
;;   :config
;;   (add-hook 'shell-mode-hook  'with-editor-export-editor)
;;   (add-hook 'eshell-mode-hook 'with-editor-export-editor)
;;   (add-hook 'term-exec-hook   'with-editor-export-editor)
;;   (add-hook 'vterm-mode-hook  'with-editor-export-editor))

(use-package hydra
  :defer t)
(use-package gdscript-mode
  :autoload gdscript-mode
  :config
  (add-hook 'gdscript-mode-hook 'flycheck-mode)
  (add-hook 'gdscript-mode-hook 'company-mode))

;; (set-face-background 'font-lock-comment-face "#fef3bd") ;; Highlight comments to make them more visible


;;=========================================================================
;; Misc
;;=========================================================================
(use-package bongo
  :autoload bongo)
;; Some LaTeX stuff
(use-package flyspell
  :autoload flyspell-mode)
(use-package writegood-mode
  :autoload writegood-mode)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'writegood-mode)
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

(provide '.emacs)
;;; .emacs ends here
