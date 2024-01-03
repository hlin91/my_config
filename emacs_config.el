(setq gc-cons-threshold 100000000) ;; Set garbage collection threshold
(setq read-process-output-max (* 1024 1024)) ;; 1mb

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
 '(ac-auto-show-menu 0.1)
 '(ac-auto-start 2)
 '(blink-cursor-mode nil)
 '(bongo-custom-backend-matchers '((mpv (local-file) . t)))
 '(bongo-enabled-backends '(mpv))
 '(bongo-mpv-extra-arguments '("--no-video"))
 '(catppuccin-flavor 'latte)
 '(company-async-redisplay-delay 0.005)
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "80214de566132bf2c844b9dee3ec0599f65c5a1f2d6ff21a2c8309e6e70f9242" "0527c20293f587f79fc1544a2472c8171abcc0fa767074a0d3ebac74793ab117" "b1acc21dcb556407306eccd73f90eb7d69664380483b18496d9c5ccc5968ab43" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "75fb82e748f32de807b3f9e8c72de801fdaeeb73c791f405d8f73711d0710856" "29b4f767c48da68f8f3c2bbf0dde2be58e4ed9c97e685af5a7ab7844f0d08b8b" "6622bb651e72d8ebd66454bd86db6c3990324243ff4325c1b6df252aba63b13e" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "6ca663019600e8e5233bf501c014aa0ec96f94da44124ca7b06d3cf32d6c5e06" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "f5e666fba0ded6ae9be004314ecf5f7feb605cdb84711b5c5ffd81acfb831183" "61526419f6ffee91cae16a75bfc0f39f7e9621280cc405edeae15699091e7c73" "b6dfff5118856529d9a410023eaa6afb825fdbf5f1bc72cda3f6f187a132de16" "86b46391e744b8fea6015224acd27e95de4c25dfd519167126e7cc5d45435864" "aae121897e4b52a7a70571028ce01a411d293d832e8733513a6e3da356ffa76e" "ec101eeff0195d92c3dc0c1c60edb1a84fa2adbbe8fdfea2a423aa95d1edc4d7" "c09c1b340d6211316dbc81500ae6cb56eab0d788ec605fb3693b869311435e0a" "8d69d4efbc29aa7ad70fc162447576c0327262c3e6baa8d7a41c90b981cc93f9" "276c08753eae8e13d7c4f851432b627af58597f2d57b09f790cb782f6f043966" "bb6d3df2670c74f4ad513512feea89cdbe3040c1d0ba00cf641d63679ee3d0fc" "5963c22b5f105090a406f6a9eff7e61aa4c64b564cabf195f3021e4752b35132" "fbd91b2e6dc5c7912e86406226638adb014612386516e078a0447195591447d3" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "ba72dfc6bb260a9d8609136b9166e04ad0292b9760a3e2431cf0cd0679f83c3a" "41098e2f8fa67dc51bbe89cce4fb7109f53a164e3a92356964c72f76d068587e" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "b186688fbec5e00ee8683b9f2588523abdf2db40562839b2c5458fcfb322c8a4" "aba75724c5d4d0ec0de949694bce5ce6416c132bb031d4e7ac1c4f2dbdd3d580" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "7922b14d8971cce37ddb5e487dbc18da5444c47f766178e5a4e72f90437c0711" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "efeac8a7737b192859b0683bb97a5c2e4c600101dccda67b78a424fc9a738b75" "4d553fbd7fa02bedfb17e8107680f56e1aa952c073f389c780a5aeaa4896867a" default))
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-ignored-server-capabilities nil)
 '(eglot-send-changes-idle-time 0.5)
 '(fast-but-imprecise-scrolling nil)
 '(gdscript-gdformat-line-length 200)
 '(global-display-line-numbers-mode t)
 '(helm-grepint-initial-case 'case-insensitive)
 '(helm-grepint-min-pattern-length 1)
 '(inhibit-startup-screen t)
 '(line-number-mode nil)
 '(nyan-animate-nyancat t)
 '(nyan-animation-frame-interval 0.1)
 '(nyan-mode t)
 '(nyan-wavy-trail t)
 '(package-selected-packages
   '(iedit fennel-mode sly racket-mode phi-search iy-go-to-char ocamlformat auto-complete company-fuzzy fzf flycheck-popup-tip flycheck-pos-tip flycheck-status-emoji flycheck-eglot popper pulsar swiper-helm markdown-preview-mode blamer auto-package-update benchmark-init undo-tree esup helm-c-yasnippet yasnippet-snippets yasnippet helm-grepint consult-flycheck consult-embark embark consult catppuccin-theme powerline all-the-icons writegood-mode minimap hydra multiple-cursors ewal-doom-themes ewal-spacemacs-themes helm emacs-async gdscript-mode ewal rainbow-mode git-gutter qml-mode projectile leuven-theme doom-themes rust-mode rainbow-delimiters ace-window use-package smex nyan-mode go-playground gotest go-errcheck bongo vterm swoop helm-swoop helm-ag elcord flycheck-google-cpplint flycheck-golangci-lint company exec-path-from-shell indent-guide neotree go-mode atom-one-dark-theme lua-mode latex-preview-pane auctex fic-mode smooth-scrolling flycheck))
 '(powerline-default-separator 'utf-8)
 '(powerline-gui-use-vcs-glyph t)
 '(select-enable-clipboard t)
 '(tool-bar-mode nil)
 '(undo-tree-enable-undo-in-region t)
 '(warning-suppress-types '((use-package) (comp) (comp))))
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
 '(popup-face ((t (:inherit default :background "gray18" :foreground "white smoke"))))
 '(popup-menu-selection-face ((t (:inherit default :background "SteelBlue3" :foreground "white"))))
 '(powerline-active0 ((t (:inherit mode-line))))
 '(powerline-active1 ((t nil)))
 '(powerline-active2 ((t (:inherit mode-line :background "SlateBlue1" :foreground "white"))))
 '(pulse-highlight-start-face ((t (:background "SlateBlue1")))))

;;=========================================================================
;; Theme and universal editor configurations
;;=========================================================================

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq byte-compile-warnings '(cl-functions))

(defun delete-inside-char (char)
  "Delete content between the specified CHAR."
  (interactive (list (read-char-from-minibuffer "Delete inside CHAR: "
                                                nil 'read-char-history)))
  (search-backward (string char))

  (let ((delete-inside-sexp (lambda () (let ((beg (+ (point) 1))) (forward-sexp) (delete-region beg (- (point) 1)) (backward-char)))))
   (cond
   ((equal char ?\[) (funcall delete-inside-sexp))
   ((equal char ?\() (funcall delete-inside-sexp))
   ((equal char ?{) (funcall delete-inside-sexp))
   ((equal char ?<) (funcall delete-inside-sexp))
   (t (progn (forward-char) (zap-up-to-char 1 char))))))

(defun mark-line ()
  "Mark the current line."
  (interactive)
  (back-to-indentation)
  (set-mark (point))
  (end-of-line current-prefix-arg))

(global-set-key (kbd "C-c M-z") #'zap-up-to-char)
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "C-c d i") #'delete-inside-char)
(global-set-key (kbd "C-c d p") #'delete-pair)
(global-set-key (kbd "C-c c") #'dabbrev-expand)
(global-set-key (kbd "C-c l") #'mark-line)
(global-set-key (kbd "C-c M-f") #'forward-sexp)
(global-set-key (kbd "C-<tab>") #'indent-rigidly-right-to-tab-stop)

;; Set up use-package for auto-installing packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'eglot)
(require 'use-package)

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(setq use-package-always-ensure t)
(use-package async)
(use-package auto-package-update
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(setq auto-window-vscroll nil)

(use-package pulsar
  :config
  (pulsar-global-mode)
  (add-hook 'helm-after-action-hook 'pulsar-recenter-middle))

(use-package hydra
  :defer t)

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

(use-package nyan-mode
  :if (display-graphic-p))

(use-package powerline
  :if (display-graphic-p))

;; (use-package ligature
;;   :if (display-graphic-p)
;;   :config
;;   (global-ligature-mode 1)
;;   (ligature-set-ligatures '(prog-mode gdscript-mode) '("->" ">=" "<=" "==" "===")))

(defun set-up-window ()
  "Configurations specific to windowed mode."
  (load-theme 'doom-spacegrey)
  (nyan-mode)
  (powerline-default-theme)
  
  (if (display-graphic-p (selected-frame))
      (progn
        (setq select-enable-clipboard 1)
        (set-frame-font "comic code ligatures:size=18")
        (set-frame-size-according-to-resolution))
    (progn
      (set-face-background 'default "unspecified-bg" (selected-frame))
      (set-face-background 'line-number "unspecified-bg" (selected-frame))
      (set-face-background 'line-number-major-tick "unspecified-bg" (selected-frame))
      (xterm-mouse-mode 1))))

(add-hook 'window-setup-hook 'set-up-window)
(add-hook 'server-after-make-frame-hook 'set-up-window)

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
  (("C-c t" . vterm)))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(use-package ace-window
  :bind
  (("M-o" . ace-window)
   ("C-c M-o" . ace-delete-window)))

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
   ("C-c SPC" . avy-goto-word-0)
   ("C-c M-SPC" . avy-goto-char-2)
   ("M-g M-g" . avy-goto-line)
   ("M-g M-w" . avy-goto-word-1)
   ("M-g M-e" . avy-goto-word-0)))

(use-package fzf
  :ensure t
  :bind
  (("C-c f" . fzf-projectile))
  :config
  (setq fzf/args "-x --preview='bat -f --number {}' --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --files-with-matches %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "ag --exclude=*.tscn --nobreak --nonumbers --noheading"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 30))

(use-package helm
  :config
  (bind-key "C-c C-h" 'helm-browse-project)
  (bind-key "C-x b" 'helm-buffers-list)
  (bind-key "C-x C-f" 'helm-find-files)
  (bind-key "C-c i" 'helm-imenu-in-all-buffers))

(use-package smex
  :bind
  (("M-x" . smex)))

(use-package helm-grepint
  :defer t
  :init
  (bind-key "C-c g" 'helm-grepint-grep-root)
  :config
  (helm-grepint-set-default-config-latest))

(use-package helm-swoop
  :defer t
  :bind
  (("C-c C-s" . helm-swoop)))

(use-package swiper
  :bind
  (("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch-backward)))

(use-package popper
  :config
  (add-to-list 'popper-reference-buffers "\\*Bongo Library\\*$")
  (add-to-list 'popper-reference-buffers "\\*Bongo Playlist\\*$")
  (add-to-list 'popper-reference-buffers "\\*helm.*\\*$")
  (add-to-list 'popper-reference-buffers "\\*Occur\\*")
  (add-to-list 'popper-reference-buffers "Grepint.*$")
  (add-to-list 'popper-reference-buffers "\\*sly-db for.*$")
  (add-to-list 'popper-reference-buffers "\\*sly-description\\*$")
  (add-to-list 'popper-reference-buffers "\\*xref\\*$")
  (popper-mode))

;; Get the backslash key back on JP keyboards
(global-set-key (kbd "M-¥") (lambda ()
                              (interactive)
                              (insert-char #x5c)
                              ))

;; (use-package xwwp
;;   :defer t
;;   :custom
;;   (xwwp-search-prefix "https://duckduckgo.com/?q="))
;; (defun dired-webkit-open ()
;;   "Open a file in xwidgets webkit from Dired mode."
;;   (interactive)
;;   (dired-copy-filename-as-kill 0)
;;   (xwidget-webkit-browse-url (concat "file://" (current-kill 0)))
;;   (let ((b (get-buffer "*xwidget-webkit*")))
;;     (if b (switch-to-buffer b))))
;; (add-hook 'dired-mode-hook (lambda ()
;;                              (local-set-key (kbd "C-c C-o") 'dired-webkit-open)))

(use-package rainbow-delimiters)

(use-package consult
  :bind
  (("M-g M-g" . consult-goto-line)))
(use-package consult-flycheck
  :defer t)

(put 'dired-find-alternate-file 'disabled nil)

(global-auto-revert-mode t)
(global-subword-mode t)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (defhydra hydra-undo (global-map "C-c u")
  "Undo tree hydra"
  ("u" undo-tree-undo "undo")
  ("r" undo-tree-redo "redo")
  ("v" undo-tree-visualize "visualize")))

(use-package iedit
  :bind
  (("C-c ;" . iedit-mode)))

;;=========================================================================
;; Coding
;;=========================================================================

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (python "https://github.com/tree-sitter/tree-sitter-phython")))

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (go-mode . go-ts-mode)
        (gdscript-mode . gdscript-ts-mode)
        (js-json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        ))

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
  :defer t)

(use-package phi-search
  :defer t)

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

;; ycmd
;; (use-package ycmd
;;   :hook
;;   (prog-mode . ycmd-mode)
;;   :config
;;   (set-variable 'ycmd-server-command `("python3" "-u" ,(file-truename "~/Repos/ycmd/ycmd"))))
;; (use-package company-ycmd)

;; Company mode auto completion settings
(use-package company
  :autoload company-mode
  :bind
  (("C-:" . company-complete-common-or-show-delayed-tooltip))
  :hook
  (prog-mode . company-mode)
  :init
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-code-ignore-case t)
  :config
  (setq-local company-backends '((company-capf company-dabbrev-code company-dabbrev company-yasnippet)))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t))

(use-package company-fuzzy
  ; :hook (company-mode . company-fuzzy-mode)
  :init
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-prefix-on-top nil
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@" "$")))

(defun setup-lightweight-company ()
  "Lightweight company setup for when capf completions are too slow."
  (company-fuzzy-mode -1)
  (company-mode -1)
  (setq-local company-backends '((company-dabbrev-code company-dabbrev company-yasnippet)))
  (company-mode 1)
  (company-fuzzy-mode 1))

;; Use corfu to manually trigger completion with capf functions (since they may be laggy)
(use-package corfu
  :init
  (global-corfu-mode 1))

(use-package cape
  ;; Bind dedicated completion commands
  :bind
  (("C-<RET>" . completion-at-point)
   ("M-<RET>" . completion-at-point)) ;; capf
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  )

(use-package flx)

;; (use-package dap-mode
;;   :defer t
;;   :hook (dap-mode . dap-ui-mode)
;;   :config
;;   (add-hook 'dap-stopped-hook
;;             (lambda (arg) (call-interactively #'dap-hydra)))
;;   (dap-register-debug-provider
;;    "gdscript"
;;    (lambda (conf)
;;      (plist-put conf :debugServer "6006")
;;      (plist-put conf :debugPort "6006")
;;      (plist-put conf :host "localhost")
;;      conf))
;;   (dap-register-debug-template "Project Skylight"
;;                                (list :type "gdscript"
;;                                      :request "launch"
;;                                      :port "6006"
;;                                      :name "Project Skylight4.0")))
(use-package flycheck
  :autoload flycheck-mode)
(use-package fic-mode
  :autoload fic-mode)
(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-mode 1))
(use-package flycheck-status-emoji
 :after (flycheck)
 :config
 (flycheck-status-emoji-mode 1))
(use-package flycheck-pos-tip
  :defer t
  :after (flycheck))

(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'prog-mode-hook #'recentf-mode)
(add-hook 'prog-mode-hook (lambda () (setq fill-column 100)))
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ;; Get line ruler column
(add-hook 'prog-mode-hook 'fic-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'show-paren-local-mode)

(defun my-go-mode-hook ()
  "Custom go hooks."
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
  :hook
  (go-mode . (lambda () (setq-default tab-width 4)))
  (go-mode . my-go-mode-hook))

(use-package rust-mode
  :autoload rust-mode
  :hook
  (rust-mode . rust-enable-format-on-save))

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

(use-package gdscript-mode
  :autoload gdscript-mode
  :hook
  (gdscript-mode . eglot-ensure)
  (gdscript-mode . setup-lightweight-company)
  :config
  (setq gdscript-godot-executable "/usr/bin/godot4")
  (keymap-unset gdscript-mode-map "C-c i" t)
  (global-flycheck-eglot-mode 1))

(use-package racket-mode
  :autoload racket-mode)

(use-package popup)

;; (defun harpoon-popup ()
;;   "Display harpoon candidates using popup."
;;   (interactive)
;;   (let* ((selection (popup-menu* (delete "" (split-string (harpoon--get-file-text) "\n"))))
;;          (full-file-name (concat (harpoon-project-root-function) selection)))
;;     (if (file-exists-p full-file-name)
;;         (find-file full-file-name)
;;       (message (concat full-file-name " not found.")))))

(defun buffer-list-popup ()
  "Switch between buffers using popup."
  (interactive)
  (let* ((selection (popup-menu* (cl-remove-if (lambda (b) (or (string-prefix-p "*" (buffer-name b))(string-prefix-p " *" (buffer-name b)))) (buffer-list)))))
    (switch-to-buffer selection)))

;; (bind-key "M-RET" 'buffer-list-popup)

;; (set-face-background 'font-lock-comment-face "#fef3bd") ;; Highlight comments to make them more visible

(setq inferior-lisp-program "sbcl --dynamic-space-size 1024")

(use-package sly
  ;; :hook  
  ;; (sly-editing-mode . company-fuzzy-mode)
  :config
  (keymap-unset sly-editing-mode-map "M-p" t)
  (keymap-unset sly-editing-mode-map "M-n" t)
  (keymap-unset sly-editing-mode-map "C-c C-k"))

(use-package fennel-mode
  :autoload fennel-mode)

;;=========================================================================
;; Hydras
;;=========================================================================
(defhydra hydra-smerge (global-map "C-c s")
  ("s" smerge-start-session "start-session")
  ("u" smerge-keep-upper "keep-upper")
  ("l" smerge-keep-lower "keep-lower")
  ("a" smerge-keep-all "keep-all")
  ("n" smerge-next "next"))

(defhydra hydra-window-resize (global-map "C-c w")
  ("w" enlarge-window "grow-vertically")
  ("s" shrink-window "shrink-vertically")
  ("d" enlarge-window-horizontally "grow-horizontally")
  ("a" shrink-window-horizontally "shrink-horizontally")
  ("p" scroll-other-window-down "scroll-other-window-down")
  ("n" scroll-other-window "scroll-other-window"))

(defhydra hydra-mc (global-map "C-c m")
  ("p" mc/mark-previous-like-this "mark-previous-like-this")
  ("n" mc/mark-next-like-this "mark-next-like-this")
  ("a" mc/edit-beginnings-of-lines "edit-beginnings-of-lines")
  ("e" mc/edit-ends-of-lines "edit-ends-of-lines")
  ("m" mc/mark-all-like-this "mark-all-like-this"))

(defhydra hydra-open (global-map "C-c o")
  ("e" (lambda() (interactive) (find-file "/home/harvey/.emacs.d/init.el")) "emacs-config")
  ("p" (lambda() (interactive) (find-file "/home/harvey/Repos/project_skylight_3d/scripts/BasePlayer.gd")) "project-skylight")
  ("b" bongo "bongo"))

(defhydra hydra-popper (global-map "C-c p")
  ("p" popper-toggle-latest "toggle-latest")
  ("f" popper-cycle "cycle-forward")
  ("b" popper-cycle-backwards "cycle-backward")
  ("k" popper-kill-latest-popup "kill-latest"))

(defhydra hydra-xref (global-map "C-c x")
  ("d" xref-find-definitions "find-definitions")
  ("r" xref-find-references "find-references"))

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

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; Artist mode shortcuts
(add-hook 'artist-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<f15>") 'org-mode)
	    (local-set-key (kbd "<f16>") 'artist-select-op-pen-line) ; f16 = pen mode
            (local-set-key (kbd "<f17>") 'artist-select-op-line)     ; f17 = line
	    (local-set-key (kbd "<f18>") 'artist-select-op-square)   ; f18 = rectangle
	    (local-set-key (kbd "<f19>") 'artist-select-op-ellipse)  ; f19 = ellipse
	    (local-set-key (kbd "C-z") 'undo)))

(provide '.emacs_config)

;;; emacs_config.el ends here
