(setq gc-cons-threshold (* 1024 1024 100)) ;; Set garbage collection threshold
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(require 'package) ;; Add the MELPA package repository
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
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
   '("b70495b8c7ece8c3fb1f6dddeeda9f7750693d28978e701f22a292232c8a5db6" "e646c19bb7708aaacf1516b73cf3e8e1d251cdefca8034fa17fa1473f610e37b" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "05ce8570993a8ef1e2b0bf939cb39fb9184cf4deed9176a20ca920463e4bd3d3" "a1169e031fe788077ea987457104ef1f6cc146b8c30fdcfd62c4b705499746a5" "da19739a11a46ef7c252d9132d0c2c08c269ef2840bb19b136d8d79e370ab1a5" "0330635316afff3aede8a591e0defc544e5ec629daded7f13bcc1290abbd0107" "92fa1d7e085c2d76af6ce4a1cf743311bc62f1ab18bb612a63af64c3fcdf7fc9" "8f5bc037e76bb9c08d0b6f1e62fadc579b029b84d1611a445d52fb0e16459c06" "ef329fbe7e802287e5007d5714db1bc691a736ef02dd2d3dd591ca3974aa671d" "ecd76ed4cdfc0534b90b663fb18afdd18bd29d7f40761f658d995c9a82f6490b" "c105fd4a19da560002ec798cb88c3589b7b0449613e53cbd28ead89a46774483" "61b4b5757df5d86d232da36a4adc6057ed56e140ac8ba827e6848103bd9e9f17" "498ce788d0564d1effb7fbd8d83c7bf636178e4ad63afa7e05d84e68503dda38" "e4ea9d3a79541c06289eec3a7dde9da89b2a70e26d2a195974841687b3614ddf" "e2bcde377ae6761652d38346e29de83015babc32d25fc9e8477bdb98883f01b2" "4df435d012a155e2f22e75b15b9f70bdf559f0305f3d7d9ea2297a89cf90e4b0" "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "c8b3d9364302b16318e0f231981e94cbe4806cb5cde5732c3e5c3e05e1472434" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40" "bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af" "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf" "d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad" "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a" "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d" "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "80214de566132bf2c844b9dee3ec0599f65c5a1f2d6ff21a2c8309e6e70f9242" "0527c20293f587f79fc1544a2472c8171abcc0fa767074a0d3ebac74793ab117" "b1acc21dcb556407306eccd73f90eb7d69664380483b18496d9c5ccc5968ab43" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "75fb82e748f32de807b3f9e8c72de801fdaeeb73c791f405d8f73711d0710856" "29b4f767c48da68f8f3c2bbf0dde2be58e4ed9c97e685af5a7ab7844f0d08b8b" "6622bb651e72d8ebd66454bd86db6c3990324243ff4325c1b6df252aba63b13e" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "6ca663019600e8e5233bf501c014aa0ec96f94da44124ca7b06d3cf32d6c5e06" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "f5e666fba0ded6ae9be004314ecf5f7feb605cdb84711b5c5ffd81acfb831183" "61526419f6ffee91cae16a75bfc0f39f7e9621280cc405edeae15699091e7c73" "b6dfff5118856529d9a410023eaa6afb825fdbf5f1bc72cda3f6f187a132de16" "86b46391e744b8fea6015224acd27e95de4c25dfd519167126e7cc5d45435864" "aae121897e4b52a7a70571028ce01a411d293d832e8733513a6e3da356ffa76e" "ec101eeff0195d92c3dc0c1c60edb1a84fa2adbbe8fdfea2a423aa95d1edc4d7" "c09c1b340d6211316dbc81500ae6cb56eab0d788ec605fb3693b869311435e0a" "8d69d4efbc29aa7ad70fc162447576c0327262c3e6baa8d7a41c90b981cc93f9" "276c08753eae8e13d7c4f851432b627af58597f2d57b09f790cb782f6f043966" "bb6d3df2670c74f4ad513512feea89cdbe3040c1d0ba00cf641d63679ee3d0fc" "5963c22b5f105090a406f6a9eff7e61aa4c64b564cabf195f3021e4752b35132" "fbd91b2e6dc5c7912e86406226638adb014612386516e078a0447195591447d3" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "ba72dfc6bb260a9d8609136b9166e04ad0292b9760a3e2431cf0cd0679f83c3a" "41098e2f8fa67dc51bbe89cce4fb7109f53a164e3a92356964c72f76d068587e" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "b186688fbec5e00ee8683b9f2588523abdf2db40562839b2c5458fcfb322c8a4" "aba75724c5d4d0ec0de949694bce5ce6416c132bb031d4e7ac1c4f2dbdd3d580" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "7922b14d8971cce37ddb5e487dbc18da5444c47f766178e5a4e72f90437c0711" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "efeac8a7737b192859b0683bb97a5c2e4c600101dccda67b78a424fc9a738b75" "4d553fbd7fa02bedfb17e8107680f56e1aa952c073f389c780a5aeaa4896867a" default))
 '(eglot-confirm-server-edits nil)
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
 '(nyan-wavy-trail t)
 '(package-selected-packages
   '(helm-rg glsl-mode darkroom markdown-mode company flx popup base16-theme orderless corfu-terminal helm-projectile fzf cape corfu goto-chg iedit sly phi-search popper pulsar swiper-helm blamer auto-package-update benchmark-init undo-tree helm-grepint hydra multiple-cursors helm emacs-async gdscript-mode rainbow-mode git-gutter qml-mode projectile doom-themes rainbow-delimiters ace-window use-package smex nyan-mode go-playground gotest go-errcheck bongo swoop helm-swoop helm-ag exec-path-from-shell go-mode lua-mode fic-mode smooth-scrolling flycheck))
 '(select-enable-clipboard t)
 '(undo-tree-enable-undo-in-region t)
 '(warning-suppress-types '((use-package) (comp) (comp)))
 '(whitespace-style
   '(face trailing tabs spaces empty indentation space-after-tab space-before-tab space-mark tab-mark)))
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
 '(avy-lead-face ((t (:background "cornflowerblue" :foreground "#212121" :weight bold))))
 '(avy-lead-face-0 ((t (:inherit avy-lead-face :background "steelblue"))))
 '(avy-lead-face-1 ((t (:inherit avy-lead-face :background "cadetblue"))))
 '(avy-lead-face-2 ((t (:inherit avy-lead-face :background "slategray"))))
 '(company-preview ((t nil)))
 '(company-preview-common ((t nil)))
 '(company-tooltip-annotation ((t (:foreground "cornflowerblue"))))
 '(company-tooltip-common ((t (:foreground "cornflowerblue" :weight bold))))
 '(company-tooltip-mouse ((t (:background "cornflowerblue" :foreground "#212121"))))
 '(company-tooltip-scrollbar-thumb ((t nil)))
 '(company-tooltip-search ((t (:weight bold))))
 '(ffap ((t (:foreground "plum"))))
 '(fill-column-indicator ((t (:foreground "#3c3c3c" :height 1))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "plum"))))
 '(highlight ((t (:background "plum" :foreground "#171F24"))))
 '(popup-face ((t (:inherit default :background "gray18" :foreground "white smoke"))))
 '(popup-menu-selection-face ((t (:inherit default :background "SteelBlue3" :foreground "white"))))
 '(pulsar-generic ((t (:inherit pulsar-yellow :extend t))))
 '(pulse-highlight-start-face ((t nil))))

;;=========================================================================
;; Custom functions, keybinds and native editor configurations
;;=========================================================================

(setq-default show-trailing-whitespace t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(setq display-line-numbers-type 'relative)
(setq byte-compile-warnings '(cl-functions))
(electric-pair-mode 1) ;; Enable electric-pair-mode
(show-paren-mode) ;; Highlight matching parentheses
(put 'dired-find-alternate-file 'disabled nil)
(global-auto-revert-mode t)
(global-subword-mode t)
(setq-default indent-tabs-mode nil) ;; Don't use tabs to indent
(setq c-basic-offset 4)
(setq auto-window-vscroll nil)
(setq package-native-compile t)
(setq frame-inhibit-implied-resize t)
(setq initial-major-mode 'fundamental-mode)
(setq-default tab-width 4)

(defun mark-line ()
  "Mark the current line."
  (interactive)
  (back-to-indentation)
  (set-mark (point))
  (end-of-line current-prefix-arg))

(defun delete-line ()
  "Delete the current line."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (kill-line))

(defun append-new-line ()
  "Insert a new line below the current line."
  (interactive)
  (end-of-line)
  (newline current-prefix-arg))

(defun delete-inside-char (char)
  "Delete content between the specified CHAR."
  (interactive (list (read-char-from-minibuffer "Delete inside CHAR: "
                                                nil 'read-char-history)))

  (if (null (search-backward (string char) (line-beginning-position) t))
      (progn
        (back-to-indentation)
        (search-forward (string char) (line-end-position))
        (backward-char)))

  (let ((delete-inside-sexp (lambda () (let ((beg (+ (point) 1))) (forward-sexp) (delete-region beg (- (point) 1)) (backward-char)))))
   (funcall delete-inside-sexp)))

(defun mark-inside-char (char)
  "Mark content between specified CHAR."
  (interactive (list (read-char-from-minibuffer "Mark inside CHAR: "
                                                nil 'read-char-history)))

  (if (null (search-backward (string char) (line-beginning-position) t))
      (progn
        (back-to-indentation)
        (search-forward (string char) (line-end-position))
        (backward-char)))

  (let ((mark-inside-sexp (lambda () (progn (mark-sexp) (forward-char) (exchange-point-and-mark) (backward-char)))))
    (funcall mark-inside-sexp)))

(defun mark-around-word ()
  "Mark the word under the cursor no matter the position."
  (interactive)
  (forward-word)
  (backward-word)
  (mark-word))

(defun delete-around-word ()
  "Delete the word under the cursor no matter the position."
  (interactive)
  (forward-word)
  (backward-word)
  (kill-word 1))

(global-set-key (kbd "M-z") #'zap-up-to-char)
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "C-c d p") (lambda () (interactive) (delete-inside-char ?\()))
(global-set-key (kbd "C-c d b") (lambda () (interactive) (delete-inside-char ?\[)))
(global-set-key (kbd "C-c d s") (lambda () (interactive) (delete-inside-char ?\")))
(global-set-key (kbd "C-c d q") (lambda () (interactive) (delete-inside-char ?\')))
(global-set-key (kbd "C-c d d") (lambda () (interactive) (delete-inside-char ?\{)))
(global-set-key (kbd "C-c d m") #'delete-pair)
(global-set-key (kbd "C-c d w") #'delete-around-word)
(global-set-key (kbd "C-c d l") #'delete-line)
(global-set-key (kbd "C-c i p") (lambda () (interactive) (mark-inside-char ?\()))
(global-set-key (kbd "C-c i b") (lambda () (interactive) (mark-inside-char ?\[)))
(global-set-key (kbd "C-c i s") (lambda () (interactive) (mark-inside-char ?\")))
(global-set-key (kbd "C-c i q") (lambda () (interactive) (mark-inside-char ?\')))
(global-set-key (kbd "C-c i d") (lambda () (interactive) (mark-inside-char ?\{)))
(global-set-key (kbd "C-c i l") #'mark-line)
(global-set-key (kbd "C-c i x") #'mark-sexp)
(global-set-key (kbd "C-c i w") #'mark-around-word)
(global-set-key (kbd "C-c c") #'dabbrev-completion)
(global-set-key (kbd "C-c M-f") #'forward-sexp)
(global-set-key (kbd "C-c DEL") #'switch-to-prev-buffer)
(global-set-key (kbd "C-c C-k") #'comment-or-uncomment-region)
(global-set-key (kbd "C-c x r") #'xref-find-references)
(global-set-key (kbd "C-c x n") #'eglot-rename)
(global-set-key (kbd "C-o") #'append-new-line)

;; Get the backslash key back on JP keyboards
(global-set-key (kbd "M-¥")(lambda ()
                             (interactive)
                             (insert-char #x5c)))

;;=========================================================================
;; Packages managed with use-package
;;=========================================================================

;; Set up use-package for auto-installing packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'eglot)
(add-to-list 'eglot-stay-out-of 'company)
(require 'use-package)

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(setq use-package-always-ensure t)
(use-package async
  :defer t)
(use-package auto-package-update
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package projectile
  :defer t
  :config
  (setq projectile-use-git-grep t))

(use-package pulsar
  :hook
  (prog-mode . pulsar-mode)
  (text-mode . pulsar-mode)
  :config
  (add-hook 'helm-after-action-hook 'pulsar-recenter-middle))

(use-package hydra
  :defer t)

(use-package darkroom
  :defer t
  :autoload (darkroom-mode)
  :config
  (setq darkroom-text-scale-increase 0)
  :hook
  (prog-mode . darkroom-tentative-mode)
  (text-mode . darkroom-tentative-mode))

(use-package base16-theme
  :defer t
  :config
  (setf base16-theme-256-color-source "colors"))

;; (use-package nyan-mode
;;   :if (display-graphic-p)
;;   :autoload (nyan-mode))

(use-package doom-themes
  :defer t)

(defun setup-window ()
  "Set up Emacs with configurations for terminal or GUI."
  (load-theme 'doom-flatwhite)
  ;; (load-theme 'base16-solarized-amber)
  (if (display-graphic-p (selected-frame))
      (progn
        ;; Configurations specific to windowed mode
        (setq select-enable-clipboard 1)
        (set-face-background 'line-number "unspecified-bg" (selected-frame))
        (set-frame-font "-????-Ark Pixel 12px Monospaced ja-regular-normal-normal-*-24-*-*-*-d-0-iso10646-1"))
    (progn
      ;; Configurations specific to terminal mode
      (set-face-background 'default "unspecified-bg" (selected-frame))
      (set-face-background 'line-number "unspecified-bg" (selected-frame))
      (set-face-background 'line-number-major-tick "unspecified-bg" (selected-frame))
      (set-face-background 'fill-column-indicator "unspecified-bg" (selected-frame))
      (xterm-mouse-mode 1))))

(add-hook 'window-setup-hook 'setup-window)
(add-hook 'server-after-make-frame-hook 'setup-window)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(use-package ace-window
  :bind
  (("M-o" . ace-window)
   ("C-c M-o" . ace-delete-window)))

;; (use-package avy
;;   :bind
;;   (("C-c C-SPC" . avy-goto-char)
;;    ("C-c SPC" . avy-goto-word-0)
;;    ("C-c M-SPC" . avy-goto-char-2)
;;    ("M-g M-g" . avy-goto-line)
;;    ("M-g M-w" . avy-goto-word-1)
;;    ("M-g M-e" . avy-goto-word-0)))

(use-package fzf
  :bind
  (("C-c f" . fzf-projectile))
  :config
  (setq fzf/args "-x --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --files-with-matches %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        fzf/grep-command "rg --no-heading -nH --ignore-files"
        ;; fzf/grep-command "ag --nobreak --nonumbers --noheading"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 30))

(use-package helm
  :bind
  (("C-c C-h" . 'helm-browse-project)
   ("C-x b" . 'helm-buffers-list)))

(use-package smex
  :bind
  (("M-x" . smex)))

(use-package helm-ag
  :defer t)
(use-package helm-rg
  :defer t)
(use-package helm-projectile
  :defer t
  :bind ; Would prefer to use rg over ag but only helm-ag has the edit result buffer feature
  (("C-c g" . helm-projectile-ag)
   ("C-c r" . helm-projectile-rg)))

(use-package helm-swoop
  :defer t
  :bind
  (("C-c C-s" . helm-swoop)))

(use-package popper
  :config
  (add-to-list 'popper-reference-buffers "\\*Bongo Library\\*$")
  (add-to-list 'popper-reference-buffers "\\*Bongo Playlist\\*$")
  (add-to-list 'popper-reference-buffers "\\*Occur\\*")
  (add-to-list 'popper-reference-buffers "Grepint.*$")
  (add-to-list 'popper-reference-buffers "\\*sly-db for.*$")
  (add-to-list 'popper-reference-buffers "\\*sly-description\\*$")
  (add-to-list 'popper-reference-buffers "\\*xref\\*$")
  (popper-mode)
  :bind
  (("C-c p" . popper-toggle-latest)))

(use-package goto-chg
  :defer t
  :bind
  (("M-," . 'goto-last-change)))

(use-package rainbow-delimiters)

(use-package undo-tree
  :hook
  (prog-mode . undo-tree-mode)
  (text-mode . undo-tree-mode)
  :init
  (defhydra hydra-undo (global-map "C-c u")
  "Undo tree hydra"
  ("u" undo-tree-undo "undo")
  ("r" undo-tree-redo "redo")
  ("v" undo-tree-visualize "visualize")))

(use-package iedit
  :bind
  (("C-c ;" . iedit-mode)))

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

(use-package multiple-cursors
  :defer t)

(use-package phi-search
  :defer t)

(use-package git-gutter
  :hook
  (prog-mode . git-gutter-mode)
  :autoload
  (prog-mode))

(use-package blamer
  :bind
  (("C-c b" . blamer-show-commit-info)))

(use-package exec-path-from-shell
  :defer t
  :config (exec-path-from-shell-initialize))

(use-package orderless
  :defer t)

(use-package flx
  :defer t)

;; Company mode auto completion settings
(use-package company
  :autoload company-mode
  :hook
  (prog-mode . company-mode)
  (org-mode . company-mode)
  :init
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-code-ignore-case nil)
  :config
  (setq-default company-backends '((company-capf company-dabbrev-code company-dabbrev company-keywords)))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq eldoc-echo-area-use-multiline-p nil))

(defun setup-lightweight-company ()
  "Lightweight company setup for when capf completions are too slow."
  (company-mode -1)
  (setq-local company-backends '((company-dabbrev-code company-dabbrev)))
  (company-mode 1))

(use-package corfu
  :defer t
  :hook
  (prog-mode . corfu-mode)
  :config
  (setq completion-styles '(basic)))

(use-package cape
  :defer t
  :bind
  (("M-<RET>" . completion-at-point)))

(use-package corfu-terminal
  :defer t
  :hook
  (corfu-mode . corfu-terminal-mode))

(use-package flycheck
  :autoload flycheck-mode)

(use-package fic-mode
  :autoload fic-mode)

(add-hook 'prog-mode-hook #'recentf-mode)
(add-hook 'prog-mode-hook (lambda () (setq fill-column 100)))
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ;; Get line ruler column
(add-hook 'prog-mode-hook 'fic-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'show-paren-local-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'whitespace-mode-hook (lambda () (set-face-background 'whitespace-tab "unspecified-bg" (selected-frame))))

(use-package go-mode
  :autoload go-mode
  :hook
  (go-mode . flycheck-mode)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'text-mode-hook 'flyspell-mode) ;; Add spell check to text mode
(add-hook 'text-mode-hook 'visual-line-mode) ;; Add line wrap to text mode
(add-hook 'text-mode-hook 'flycheck-mode)
(add-hook 'text-mode-hook (lambda () (setq fill-column 200)))

(use-package gdscript-mode
  :autoload gdscript-mode
  :hook
  (gdscript-mode . eglot-ensure)
  (gdscript-mode . setup-lightweight-company) ; Because Godot's LSP server is slow and does not give many suggestions
  :bind
  (("C-c C-l" . gdscript-completion-insert-file-path-at-point)
   ("C-<right>" . gdscript-indent-shift-right)
   ("C-<left>" . gdscript-indent-shift-left))
  :config
  (setq gdscript-godot-executable "/usr/bin/godot")
  (keymap-unset gdscript-mode-map "C-c i" t))

(use-package glsl-mode
  :autoload glsl-mode
  :hook
  (glsl-mode . indent-tabs-mode))

(use-package lua-mode
  :autoload lua-mode)

(use-package popup
  :defer t)

;; (set-face-background 'font-lock-comment-face "#fef3bd") ;; Highlight comments to make them more visible

(setq inferior-lisp-program "sbcl --dynamic-space-size 1024")

(use-package sly
  :autoload (sly-mode)
  :config
  (keymap-unset sly-editing-mode-map "M-p" t)
  (keymap-unset sly-editing-mode-map "M-n" t)
  (keymap-unset sly-editing-mode-map "C-c C-k")
  (keymap-unset sly-editing-mode-map "C-c i w")
  (keymap-unset sly-editing-mode-map "C-c i x")
  (keymap-unset sly-editing-mode-map "C-c i p")
  (keymap-unset sly-editing-mode-map "C-c i l"))

;;=========================================================================
;; Hydras
;;=========================================================================
(defhydra hydra-smerge (global-map "C-c s")
  ("s" smerge-start-session "start-session")
  ("u" smerge-keep-upper "keep-upper")
  ("l" smerge-keep-lower "keep-lower")
  ("a" smerge-keep-all "keep-all")
  ("n" smerge-next "next"))

(defhydra hydra-mc (global-map "C-c m")
  ("p" mc/mark-previous-like-this "mark-previous-like-this")
  ("n" mc/mark-next-like-this "mark-next-like-this")
  ("a" mc/edit-beginnings-of-lines "edit-beginnings-of-lines")
  ("e" mc/edit-ends-of-lines "edit-ends-of-lines")
  ("m" mc/mark-all-like-this "mark-all-like-this")
  ("q" nil "quit"))

(defhydra hydra-open (global-map "C-c o")
  ("e" (lambda() (interactive) (find-file "/home/harvey/.emacs.d/init.el")) "emacs-config")
  ("p" (lambda() (interactive) (find-file "/home/harvey/Repos/project_skylight_3d/scripts/turn_based_scripts/Actor.gd")) "project-skylight")
  ("o" (lambda () (interactive) (find-file "/home/harvey/org/notes.org")) "org-notes")
  ("b" bongo "bongo"))

;;=========================================================================
;; Prose
;;=========================================================================
(use-package bongo
  :defer t
  :autoload bongo)

(use-package flyspell
  :defer t
  :autoload flyspell-mode)

(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(setf org-agenda-files '("/home/harvey/org/"))

(add-hook 'artist-mode-hook
          (lambda ()
            (local-set-key (kbd "<f1>") 'org-mode)
            (local-set-key (kbd "<f2>") 'artist-select-op-pen-line) ; f2 = pen mode
            (local-set-key (kbd "<f3>") 'artist-select-op-line)     ; f3 = line
            (local-set-key (kbd "<f4>") 'artist-select-op-square)   ; f4 = rectangle
            (local-set-key (kbd "<f5>") 'artist-select-op-ellipse)  ; f5 = ellipse
            (local-set-key (kbd "C-z") 'undo)
            ))

(provide '.emacs_config)

;;; emacs_config.el ends here
