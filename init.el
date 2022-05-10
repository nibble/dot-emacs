;;--------------------------  -*- coding: utf-8; mode: emacs-lisp; -*-
;; ~/.emacs.d/init.el              Javier Donaire <jdonaire@gmail.com>
;;--------------------------------------------------------------------

;;--------------------------------------------------------------------
;;  configurations that have to be applied before a library is loaded
;;--------------------------------------------------------------------

;; undo some of the winluser-friendly changes introduced in emacs23
(when (>= emacs-major-version 23)
  (setq transient-mark-mode nil
        shift-select-mode nil
        org-replace-disputed-keys t
        initial-scratch-message nil))

;; add to the load-path local directory for dropped-in elisp files
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; ensure ~/.emacs.d/cache directory exists
(setq user-emacs-cache-directory (expand-file-name "cache/" user-emacs-directory))
(when (not (file-directory-p user-emacs-cache-directory))
  (make-directory user-emacs-cache-directory nil))

;; elpa
(when (>= emacs-major-version 24)
  (require 'package)

  ;; prevent writing package-selected-packages custom variable
  (defun package--save-selected-packages (&optional value) nil)

  ;; disable TLS 1.3 support on emacs < 26.3 as workaround to
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
  (when (or (< emacs-major-version 26)
            (and (= emacs-major-version 26) (< emacs-minor-version 3)))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

  ;; disable repository signature check (uncomment if needed)
  ;; (setq package-check-signature nil)

  ;; set repositories
  (setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ("org" . "https://orgmode.org/elpa/")))

  (package-initialize)
  (setq package-enable-at-startup nil))

;; force a package refresh when the first package of a session is installed
;; https://github.com/jwiegley/use-package/issues/256
(defun my/package-install-refresh-contents (&rest args)
  (package-refresh-contents)
  (advice-remove 'package-install 'my/package-install-refresh-contents))
(advice-add 'package-install :before 'my/package-install-refresh-contents)

;; ensure use-package is installed and configure it
(unless (package-installed-p 'use-package)
  ;; (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose nil)


;;--------------------------------------------------------------------
;;  loading of libraries
;;--------------------------------------------------------------------

;; workaround to make dead keys work
;; it also works deleting envvar: XMODIFIERS= emacs
(require 'iso-transl)

;; this file uses some dired functions
(require 'dired)

;; load dired-x to expand dired capabilities
(require 'dired-x)

;; used by use-package to remove mode line displays of minor modes
(use-package diminish :ensure t)

;; configure emacs server
(use-package server
  :defer t
  :config
  (setq server-auth-dir (expand-file-name "server" user-emacs-cache-directory)))

;; configure abbrev-mode
(use-package abbrev
  :defer t
  :diminish abbrev-mode
  :config
  (setq save-abbrevs nil))

;; move and kill-word by each CamelCase subword
(use-package subword
  :defer t
  :diminish subword-mode
  :hook (prog-mode . subword-mode))

;; configure hideshow to fold regions
(use-package hideshow
  :defer t
  :diminish hs-minor-mode
  :config
  (setq hs-hide-comments nil  ;; also hide comments with hs-hide-all
		hs-isearch-open 'x)   ;; isearch enters hidden text blocks
  :hook (prog-mode . hs-minor-mode)
  :bind
  ;; f8 & f9, C-f8 & C-f9, for hs-minor-mode folding
  ("<f8>" . hs-hide-block)
  ("<f9>" . hs-show-block)
  ("C-<f8>" . hs-hide-all)
  ("C-<f9>" . hs-show-all))

;; easy-kill replaces M-w with different actions, ? for help
(use-package easy-kill :ensure t
  :config
  (setq transient-mark-mode t)  ; unfortunately it doesn't work if disabled :(
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark)
  ;; use word-strictly to select whole words even if subword-mode is active
  (setcar easy-kill-alist '(?w word-strictly " ")))

;; don't activate the region with C-x C-x, needed with transient-mark-mode
;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun my/exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'my/exchange-point-and-mark-no-activate)

;; third party emacs mode for using global tags
(when (>= emacs-major-version 25)
  (use-package ggtags :ensure t
    :diminish ggtags-mode
    :bind ("C-." . ggtags-find-reference)))

;; eldoc prints in the minibuffer the definition of the function at point.
;; Activation needed only for older emacs versions, as it is now globally
;; enabled by default
(use-package eldoc
  :diminish eldoc-mode
  :config
  (setq eldoc-idle-delay 0.5)
  (when (< emacs-major-version 25)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'c-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'c++-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'python-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'cperl-mode-hook (lambda ()
                                 (set (make-local-variable 'eldoc-documentation-function)
                                      (lambda () (let ((cperl-message-on-help-error nil))
                                                   (cperl-get-help))))
                                 (turn-on-eldoc-mode)))))

;; load markdown edition mode and configure it to use pandoc
;; inspired by https://gist.github.com/fredRos/0e3a845de95ec654538f
(use-package markdown-mode :ensure t
  :defer t
  :config
  (setq markdown-command (concat "pandoc -f markdown -t html5 -c "
                                 (expand-file-name "res/github-pandoc.css" user-emacs-directory)
                                 " --self-contained --standalone")))

;; same name buffers are identified by its directory instead of <n>
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; remove trailing whitespace on save only on the touched lines
(use-package ws-butler :ensure t
  :diminish ws-butler-mode
  :config (ws-butler-global-mode 1))

;; remember point position on closing files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "places" user-emacs-cache-directory)
        save-place-forget-unreadable-files nil)
  (if (>= emacs-major-version 25)
      (save-place-mode 1)
    (setq-default save-place t)))

;; use nyan cat modeline indication instead of scrollbars
(use-package nyan-mode :ensure t
  :config
  (when (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (setq nyan-bar-length 10)
  (nyan-mode t))

;; M-x rainbow-mode to print color strings with colored background
(use-package rainbow-mode :ensure t
  :defer t
  :hook css-mode)

;; show parenthesis with different colors
(use-package rainbow-delimiters :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; js2-mode for javascript
(use-package js2-mode :ensure t
  :defer t
  :config
  (setq js-basic-indent 2)
  (setq-default js2-basic-indent 2
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-bounce-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "console" "JSON" "jQuery" "$"))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;; web-mode for html
(use-package web-mode :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xhtml\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; ledger-mode
(use-package ledger-mode :ensure t
  :defer t
  :config
  (setq ledger-default-date-format "%Y-%m-%d"
        ledger-use-iso-dates t
        ledger-reconcile-default-commodity "EUR"
        ledger-narrow-on-reconcile nil
        ledger-highlight-xact-under-point nil))

;; org-mode
(use-package org
  :ensure org-plus-contrib
  :pin org
  :mode ("\\.org$" . org-mode)
  :config
  ;; increase number of lines in which emphasis markup will be applied
  (setcar (nthcdr 4 org-emphasis-regexp-components) 6)
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  ;; change to a light theme when exporting, so the syntax highlight is visible
  ;; with a white background
  ;; https://correl.phoenixinquis.net/2016/02/23/org-publish-with-theme.html
  (defun my/with-theme (theme fn &rest args)
    (let ((current-themes custom-enabled-themes))
      (mapcar #'disable-theme custom-enabled-themes)
      (load-theme theme t)
      (let ((result (apply fn args)))
        (mapcar #'disable-theme custom-enabled-themes)
        (mapcar (lambda (theme) (load-theme theme t)) current-themes)
        result)))
  (advice-add #'org-export-to-file :around (apply-partially #'my/with-theme 'tsdh-light))
  (advice-add #'org-export-to-buffer :around (apply-partially #'my/with-theme 'tsdh-light))
  ;; load additional export methods
  (require 'ox-odt nil t)
  (require 'ox-md nil t)
  (require 'ox-freemind nil t)
  (require 'ox-texinfo nil t)
  ;; enable org-babel languages
  (org-babel-do-load-languages 'org-babel-load-languages
                               '(;; other Babel languages
                                 (plantuml . t))))

;; htmlize is used by org-mode on export to syntax highlight source code blocks
(use-package htmlize :ensure t
  :defer t)

;; move current line or region with M-up / M-down
(use-package move-text :ensure t
  :if (>= emacs-major-version 25)
  :config
  (move-text-default-bindings))

;; configure recentf to increase the history of ivy virtual buffers
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-cache-directory)
        recentf-max-saved-items 200))

;; load and configure ivy/swiper/counsel completion framework
(use-package wgrep :ensure t)
(use-package hydra :ensure t)
(use-package counsel :ensure t)
(use-package ivy-hydra :ensure t)
(use-package smex :ensure t
  :config
  (setq smex-save-file (expand-file-name "smex-items" user-emacs-cache-directory)))
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :init
  (ivy-mode 1)
  :config
  (setq ivy-height 20
        ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-dynamic-exhibit-delay-ms 200)
  ;; Update which-function after a match is visualized without closing ivy
  ;; (C-M-m, C-M-n, C-M-p) or in swiper with each highlighted result
  (defun my/which-func-update-ivy () (which-func-update-1 (ivy--get-window ivy-last)))
  (advice-add 'ivy-call :after #'my/which-func-update-ivy)
  (advice-add 'swiper--update-input-ivy :after #'my/which-func-update-ivy)
  :bind
  ("C-c M-x" . execute-extended-command)
  ("C-s" . swiper)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c f" . counsel-recentf)
  ("C-c g" . counsel-git)
  ("C-c p" . counsel-git-grep)
  ("C-c k" . counsel-rg)
  ("C-c l" . counsel-file-jump)
  ("C-c r" . ivy-resume)
  ("M-y" . counsel-yank-pop))

;; load and configure magit
(when (>= emacs-major-version 25)
  (use-package transient :ensure t
    :defer t
    :init
    (setq transient-levels-file (expand-file-name "transient/levels.el" user-emacs-cache-directory)
          transient-values-file (expand-file-name "transient/values.el" user-emacs-cache-directory)
          transient-history-file (expand-file-name "transient/history.el" user-emacs-cache-directory)))

  (use-package magit :ensure t
    :defer t
    :bind
    ("C-x g" . magit-status)
    :config
    ;; ensure buffers are always shown in other window
    (defun my/magit-display-file-buffer (buffer)
      (pop-to-buffer buffer t))
    (setq magit-display-file-buffer-function 'my/magit-display-file-buffer))
  )

;; zenburn low contrast dark theme
(use-package zenburn-theme :ensure t
  :defer t
  :config
  (custom-theme-set-faces
   'zenburn
   `(highlight    ((t (:background "#494949"))))  ; zenburn-bg+05
   `(hl-line      ((t (:background "#494949"))))  ; zenburn-bg+05
   `(org-level-7  ((t (:foreground "#ECB3B3"))))  ; zenburn-red+2
   `(org-archived ((t (:foreground "#656555"))))  ; zenburn-fg-1
   ))

;; modus-operandi light theme
(when (>= emacs-major-version 26)
  (use-package modus-operandi-theme :ensure t
    ;; :defer t
    :init
    (setq modus-operandi-theme-fringes 'subtle
          modus-operandi-theme-intense-paren-match t
          modus-operandi-theme-rainbow-headings t
          )))

;; modus-vivendi dark theme
(when (>= emacs-major-version 26)
  (use-package modus-vivendi-theme :ensure t
    :defer t
    :init
    (setq modus-vivendi-theme-fringes 'subtle
          modus-vivendi-theme-intense-paren-match t
          modus-vivendi-theme-rainbow-headings t
          )))

;; load and configure graphviz mode
(use-package graphviz-dot-mode :ensure t
  :defer t
  :config
  (setq graphviz-dot-auto-indent-on-braces nil
        graphviz-dot-auto-indent-on-newline nil
        graphviz-dot-auto-indent-on-semi nil))

;; edit PlatUML diagrams
(when (>= emacs-major-version 25)
  (use-package plantuml-mode :ensure t
    :defer t
    :mode "\\.puml$\\'"
    :config
    ;; set the path of PlantUML jar file to what is used by the Debian package
    (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
    ;; set the path of PlantUML jar file for org
    (setq org-plantuml-jar-path plantuml-jar-path)))

;; ignore mouse, not enabled here
(use-package disable-mouse :ensure t
  :defer t
  :config
  (setq disable-mouse-mode-global-lighter ""))

;; disk usage tool
(when (>= emacs-major-version 26)
  (use-package disk-usage :ensure t
    :defer t))

;; git-timemachine to browse different file revisions (n|ext, p|revious,
;; q|uit). Launch with M-x git-timemachine or with C-x v t (M-x
;; counsel-git-timemachine) for ivy-like interface
(when (>= emacs-major-version 25)
  (use-package git-timemachine :ensure t
    :defer t
    :config
    (setq git-timemachine-abbreviation-length 8)
    ;; ivy integration
    ;; http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
    (defun counsel-git-timemachine-show-selected-revision ()
      "Show last (current) revision of file."
      (interactive)
      (let* ((collection (mapcar (lambda (rev)
                                   ;; re-shape list for the ivy-read
                                   (cons (concat (substring-no-properties (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                                 (git-timemachine--revisions))))
        (ivy-read "commits:"
                  collection
                  :action (lambda (rev)
                            ;; compatible with ivy 9+ and ivy 8
                            (unless (string-match-p "^[a-z0-9]*$" (car rev))
                              (setq rev (cdr rev)))
                            (git-timemachine-show-revision rev)))))

    (defun counsel-git-timemachine ()
      "Open git snapshot with the selected version.  Based on ivy-mode."
      (interactive)
      (unless (featurep 'git-timemachine)
        (require 'git-timemachine))
      (git-timemachine--start #'counsel-git-timemachine-show-selected-revision))
    :bind
    ("C-x v t" . counsel-git-timemachine)))

;; racket mode
(when (>= emacs-major-version 25)
  (use-package racket-mode :ensure t
    :defer t))

;; forth mode
(use-package forth-mode :ensure t
  :defer t)

;; golang mode
(use-package go-mode :ensure t
  :defer t)

;; rust mode
(use-package rust-mode :ensure t
  :defer t)

;; toml mode
(use-package toml-mode :ensure t
  :defer t)

;; yaml mode
(use-package yaml-mode :ensure t
  :defer t)

;; multi-term to have several term buffers
(use-package multi-term :ensure t
  :defer t)

;; edit files in hexadecimal
(use-package nhexl-mode :ensure t
  :defer t)

;; zoom-frm (local lisp files) to zoom whole frame instead of buffer text
(use-package frame-fns)
(use-package frame-cmds)
(use-package zoom-frm
  :config
  (define-key ctl-x-map [(control ?+)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?-)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?=)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?0)] 'zoom-in/out))

;; configure emacs calendar
(use-package calendar
  :defer t
  :config
  ;; set european style, and show week numbers
  (setq calendar-week-start-day 1
        european-calendar-style 't
        calendar-intermonth-text '(propertize
                                   (format "%2d"
                                           (car (calendar-iso-from-absolute
                                                 (calendar-absolute-from-gregorian (list month day year)))))
                                   'font-lock-face 'font-lock-function-name-face))
  ;; Use 'y' to copy the date under the cursor, close the calendar, and yank it
  ;; into the buffer. Inspired by Ian Yang's iy/calendar-copy-date
  (defun my/calendar-copy-date ()
    (interactive)
    (let ((date (calendar-cursor-to-date t))
          (format "%Y-%m-%d"))
      (setq date (encode-time 0 0 0 (cadr date) (car date) (nth 2 date)))
      (setq string (format-time-string format date))
      (kill-new string)
      (calendar-exit)
      (yank)))
  (define-key calendar-mode-map (kbd "y") 'my/calendar-copy-date))

;; configure emacs web browser
(use-package eww
  :defer t
  :config
  ;; https://emacs.stackexchange.com/questions/561/how-can-i-toggle-displaying-images-in-eww-without-a-page-refresh
  (defun my/eww-toggle-images ()
    "Toggle whether images are loaded and reload the current page from cache."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (eww-reload t)
    (message "Images are now %s" (if shr-inhibit-images "off" "on")))
  (define-key eww-mode-map (kbd "I") #'my/eww-toggle-images)
  (define-key eww-link-keymap (kbd "I") #'my/eww-toggle-images)
  (setq-default shr-inhibit-images t)  ; hide images by default, toggle with `I`
  (setq-default shr-use-fonts nil)     ; always use the editor default font, toggle with `F`
)

;; git-gutter marks modified chunks in the file and performs some git commands.
;; It should be installed at the end because it will analyse any open file and
;; crash in Windows when installing from scratch, as it has to popen git for
;; every installed library file (org-mode specially)
(use-package git-gutter :ensure t
  :diminish (git-gutter-mode . "")
  :config
  (global-git-gutter-mode t)
  :bind
  ("C-x v p" . git-gutter:previous-hunk)
  ("C-x v n" . git-gutter:next-hunk)
  ("C-x v s" . git-gutter:stage-hunk))

;; git-gutter-fringe places git-gutter indications at the fringe in graphics
;; mode
(use-package git-gutter-fringe :ensure t
  :if (boundp 'fringe-mode))


;;--------------------------------------------------------------------
;;  local settings in ~/.emacs.d/local.el file
;;--------------------------------------------------------------------

;; default fonts
(defvar cfg-font-ttf "monospace-10")
(when (eq system-type 'windows-nt)
  (setq cfg-font-ttf "Consolas 10"))

(defvar cfg-font-x "-misc-fixed-medium-r-normal--*-*-*-*-*-90-iso8859-*")

;; settings loaded from local.el
(load (expand-file-name "local.el" user-emacs-directory) t)

;; example of local.el:
;; (setq cfg-font-ttf "inconsolata-10"
;;       cfg-font-x "-*-terminus-medium-r-*-*-*-*-*-*-*-60-iso8859-*")

;; example of local.el for Ms Windows:
;; (setq cfg-font-ttf "Consolas 9")
;; (setenv "PATH"
;;         (concat "C:/Program Files/Git/usr/bin" ";" (getenv "PATH")))

;; set selected font
(cond ((>= emacs-major-version 23)
       (add-to-list 'default-frame-alist `(font . ,cfg-font-ttf)))
      ((>= emacs-major-version 22)
       (add-to-list 'default-frame-alist `(font . ,cfg-font-x)))
      ((window-system)
       (set-face-font 'default cfg-font-x)))


;;--------------------------------------------------------------------
;;  custom launch switchs
;;--------------------------------------------------------------------

;; ediff (bojohan): emacs -diff file1 file2
(defun my/command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))
(add-to-list 'command-switch-alist '("-diff" . my/command-line-diff))


;;--------------------------------------------------------------------
;;  miscellaneous settings
;;--------------------------------------------------------------------

;; enable CUA mode, with as much of its functionality as possible
;; disabled, to get column editing functionality in emacs < 24.4
;; see notes section at the end
;; (if (or (< emacs-major-version 24)
;;         (and (= emacs-major-version 24) (< emacs-minor-version 4)))
;;     (progn (setq cua-enable-cua-keys nil
;;                  cua-highlight-region-shift-only t
;;                  cua-delete-selection nil
;;                  cua-toggle-set-mark nil
;;                  cua-enable-modeline-indications t)
;;            (cua-mode t)))

;; fix oddities provoked by the new window management of emacs24
(when (boundp 'display-buffer-alist)
  (setq display-buffer-alist
        '(
          ;; don't ever raise gdb/gud output buffer in the current frame
          ;;
          ;; it's not the only emacs 24 annoyance, emacs gdb-mi crashes with
          ;; threaded bins: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12425
          ;;
          ;; I'll keep using emacs23 for debugging
          ;;
          ("\\*input\\/output of .*"
           (display-buffer-reuse-window)
           (reusable-frames . t))
          )))

;; fix some cases in which a new emacsclient frame appears below other
;; windows in gnome 3
;; https://askubuntu.com/questions/283711/application-focus-of-emacsclient-frame
(defun my/px-raise-frame-and-give-focus ()
  (when window-system
    (raise-frame)
    ;; (x-focus-frame (selected-frame))
    ;; (set-mouse-pixel-position (selected-frame) 4 4)
    ))
(add-hook 'server-switch-hook 'my/px-raise-frame-and-give-focus)

;; frame title format "name of buffer [emacsXY]"
(setq frame-title-format (format "%%b [emacs%d]" emacs-major-version))

;; hide the menu
(menu-bar-mode -1)

;; hide the tool bar, protected because emacs could be compiled without X
(when (boundp 'tool-bar-mode)
  (ignore-errors (tool-bar-mode -1)))

;; cursor doesn't blink
(blink-cursor-mode -1)

;; enable narrow
(put 'narrow-to-region 'disabled nil)

;; syntax highlight
(global-font-lock-mode t)

;; support ansi color in shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; prevent removing of prompt characters in shell modes
(setq comint-prompt-read-only t)

;; ignore duplicate commands when browsing history in shell modes
(setq comint-input-ignoredups t)

;; configure scrolling in shell modes
;;(setq comint-scroll-show-maxiumum-output)
(setq comint-scroll-to-bottom-on-input t)

;; show the column number at the point in the mode line
(column-number-mode t)

;; show aprox file size in the mode line
(size-indication-mode t)

;; show name of function at point in mode line
(which-function-mode)

;; highlight the opposite parenthesis/bracket/etc to the one at the point
(show-paren-mode 1)

;; replace audio warnings for visual warnings
(setq visible-bell t)

;; don't show welcome message
(setq inhibit-startup-message t)

;; accept 'y' or 'n' instead of 'yes' or 'no' as questions answer
(fset 'yes-or-no-p 'y-or-n-p)

;; warn when opening files bigger than 100 MB instead of the default of 10 MB
(setq large-file-warning-threshold 100000000)

;; auto revert unmodified files periodically
(progn (setq auto-revert-interval 5)
       (global-auto-revert-mode t))

;; open links to files in version control without asking
(setq vc-follow-symlinks nil)

;; prevents M-z to minimize frame
(defun iconify-or-deiconify-frame (&optional frame)
  (message "Iconify key disabled, use M-x iconify-frame"))

;; C-x C-b runs ibuffer instead of list-buffers
(defalias 'list-buffers 'ibuffer)

;; activate windmove, change windows with shift-(cursors)
(windmove-default-keybindings)

;; recover windows placement with winner-undo/winner-redo, don't use
;; keybindings C-c left/right to have them available for picture-mode
(progn (setq winner-dont-bind-my-keys t)
       (winner-mode 1))

;; put auto-save-list directory inside ~/.emacs.d/cache
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" user-emacs-cache-directory))

;; set history file location to ~/.emacs.d/cache/history
(setq savehist-file
      (expand-file-name "history" user-emacs-cache-directory))

;; put url cache directory inside ~/.emacs.d/cache
(setq url-configuration-directory
      (expand-file-name "url" user-emacs-cache-directory))

;; put game scores directory inside ~/.emacs.d/cache
(setq gamegrid-user-score-file-directory
      (expand-file-name "games" user-emacs-cache-directory))

;; keep minibuffer history
(savehist-mode 1)

;; utf-8 is the default charset
(prefer-coding-system 'utf-8-unix)

;; increase kill ring size (default 60)
(setq kill-ring-max 200)

;; inhibit autosave
;; (setq auto-save-default nil)

;; set directory for temporary files
;; disabled, it breaks EasyPG
;; (setq temporary-file-directory "~/.emacs.d/tmp/")

;; don't use external gpg agent to prompt for passwords
(setenv "GPG_AGENT_INFO" nil)

;; try:
;; (defadvice epg--start (around advice-epg-disable-agent disable)
;;   "Make epg--start not able to find a gpg-agent"
;;   (let ((agent (getenv "GPG_AGENT_INFO")))
;;     (setenv "GPG_AGENT_INFO" nil)
;;     ad-do-it
;;     (setenv "GPG_AGENT_INFO" agent)))

;; never use tabs for indenting
(setq-default indent-tabs-mode nil)

;; backspace removes tabs instead of untabifying and removing just one space
(setq backward-delete-char-untabify-method nil)

;; period single space ends sentence
(setq sentence-end-double-space nil)

;; show arrows when visual-line-mode wraps lines
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;; M-x customize displays real variable names
(setq custom-unlispify-tag-names nil
      custom-unlispify-menu-entries nil)

;; spell checking with aspell
(setq ispell-program-name "aspell"
      ispell-dictionary "american")

;; scroll with 1-line steps
(setq scroll-step 1
      scroll-preserve-screen-position t
      scroll-conservatively 99999)

;;scroll window up/down by one line
;; (global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
;; (global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; always leave some lines of context when the cursor is moved to the top/bottom
(setq scroll-margin 2)

;; drag scrollbar with left mouse button, for athena toolkit
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

;; middle mouse button paste at cursor position instead of mouse position
(setq mouse-yank-at-point t)

;; after copy Ctrl+c in X11 apps, paste it using `yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, paste it using `yank' in emacs
(setq x-select-enable-primary t)

;; set a better fill column default value
(setq-default fill-column 79)

;; don't add newlines at the end of the file when point reaches it
(setq next-line-add-newlines nil)

;; never overflow lines
(toggle-truncate-lines 1)
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

;; settings for compile mode
(setq compilation-scroll-output t
      compilation-window-height 16)

;; set default settings for grep function
;; (setq grep-command "grep -nHIr -e ")

;; enable downcase-region and upcase-region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; don't open a new frame for ediff control
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; split ediff windows horizontally if the frame width is enough
(setq ediff-split-window-function
      (lambda (&optional arg)
        (if (> (frame-width) 160)
            (split-window-horizontally arg)
          (split-window-vertically arg))))

;; make dired guess target directory for Copy or Rename operations,
;; usually the other dired buffer shown, so it works like a FTP client
(setq dired-dwim-target t)

;; toggle hiding dot files in dired with C-x M-o
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))

;; dabbrev completion matches case
(setq dabbrev-case-fold-search nil
      dabbrev-case-replace nil)

;; hippie completion configuration
(setq case-replace nil
      hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
;; ensure hippie-expand searches match case
;; https://stackoverflow.com/questions/8722301/force-hippie-expand-to-respect-capitalization
(defadvice hippie-expand (around hippie-expand-case-fold)
  "Try to do case-sensitive matching (not effective with all functions)."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'hippie-expand)

;; set cache dir for semantic so it doesn't leave all its junk everywhere
(setq semanticdb-default-save-directory
      (expand-file-name "semantic.cache" user-emacs-cache-directory))

;; constant width of unix manual pages viewer
(setq Man-width 79)

;; diverse configurations to emulate Microsoft Visual Studio or improve usage in Windows
(if (eq system-type 'windows-nt)
    (progn
      ;;
      ;; more suitable behaviour in windows
      (prefer-coding-system 'utf-8-dos)
      (global-disable-mouse-mode)  ; ignore mouse to compensate slopy focus missing in Windows desktop
      (setq browse-url-browser-function 'browse-url-chrome)
      ;;
      ;; speed up emacs in windows
      (setq auto-save-default nil)
      (setq make-backup-files nil)
      (setq ggtags-highlight-tag nil)  ; deactivated because it is too slow in windows
      (setq ggtags-oversize-limit (* 1 1024 1024))  ; reduce threshold to update whole GTAGS
      (setq counsel--git-grep-count-threshold -1)  ; don't preload every git grep result on invocation, terrible for huge repos
      (when (>= emacs-major-version 25)
        (setq inhibit-compacting-font-caches t))  ; mitigate slowdowns with undisplayable unicode chars
      ;;
      ;; replace find with rg so counsel-file-jump works in Windows (need rg.exe in %PATH%)
      (setq find-program "rg")
      (setq counsel-file-jump-args (split-string "--files --no-ignore ."))
      ;;
      ;; workaround if common unix commands or git can't be run (ex: magit)
      ;; (setq explicit-shell-file-name
      ;;       "C:/Program Files/Git/bin/bash.exe")
      ;; (setq shell-file-name explicit-shell-file-name)
      ;; (add-to-list 'exec-path "C:/Program Files/Git/bin")
      ;;
      ;; make C indenting similar to Visual Studio
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (setq indent-tabs-mode t)
                  (setq tab-width 4)
                  ;; (setq buffer-display-table (make-display-table))
                  ;; (aset buffer-display-table ?\^M [])  ; hide ^M when a file has both unix and dos line endings
                  ))
      (setq my-c-style-to-use "microsoft"))
  ;;
  ;; set default C style when not in windows
  (setq my-c-style-to-use "stroustrup"))

;; C style that emulates Microsoft Visual Studio
(c-add-style "microsoft"
             '("stroustrup"
               (c-offsets-alist
                (innamespace . -)
                (inline-open . 0)
                (inher-cont . c-lineup-multi-inher)
                (arglist-cont-nonempty . +)
                (template-args-cont . +)
                (case-label . +))))

;; settings for c derivates modes
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style my-c-style-to-use)
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\|ASSUMPTION\\):"
                1 font-lock-warning-face t)))))

;; settings for c++ mode
(defun my/c++-setup ()
  (c-set-offset 'innamespace '0)    ;; don't indent inside namespace
  (c-set-offset 'inextern-lang '0)  ;; don't indent inside extern
  (c-set-offset 'inline-open '0))   ;; don't indent the opening bracket of a method
(add-hook 'c++-mode-hook 'my/c++-setup)

;; default compile command
(setq compile-command "make")

;; use ruby-mode for some Ruby internal DSL files
(setq auto-mode-alist (cons '("\\WRakefile$" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\WGemfile"   . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.rake$"     . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.gemspec$"  . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.jbuilder$" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.shaper$"   . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.ruby$"     . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.ru$"       . ruby-mode) auto-mode-alist))

;; prevent emacs from inserting utf-8 coding information in ruby files
(setq ruby-insert-encoding-magic-comment nil)

;; use cperl-mode instead of perl-mode
(fset 'perl-mode 'cperl-mode)

;; more coherent indent for cperl-mode
(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)

;; configure fill-paragraph for Python docstrings
(setq python-fill-docstring-style 'django)

;; extensions for ada-mode
(setq auto-mode-alist (cons '("\\.pkb\\.a$" . ada-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.pks\\.a$" . ada-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.pro\\.a$" . ada-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.tsk\\.a$" . ada-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.a$" . ada-mode) auto-mode-alist))

;; settings for ada-mode so it is less intrusive
(setq ada-auto-case nil
      ada-broken-decl-indent 0
      ada-broken-indent 2
      ada-clean-buffer-before-saving nil
      ada-indent 4
      ada-indent-after-return nil
      ;; ada-indent-comment-as-code nil
      ada-indent-comment-as-code t
      ada-indent-record-rel-type 4
      ada-language-version 'ada83
      ada-which-compiler 'generic
      ada-fill-comment-prefix "-- ")

;; use octave-mode for files with extension .m
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; don't ask when loading files with some file local variables
(setq safe-local-variable-values '((org-confirm-babel:evaluate . nil)))

;; open files with .org extension with org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; keywords for org-mode
(setq org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "DELEGATED" "REVIEW"
                                    "|" "REJECTED" "DONE" "ARCHIVED"))
      ;; org-todo-keyword-faces '(("TODO"      . (:foreground "magenta" :weight bold))
      ;;                          ("DOING"     . (:foreground "orange" :weight bold))
      ;;                          ("NEEDINFO"  . (:foreground "red" :weight bold))
      ;;                          ("DELEGATED" . (:foreground "blue" :weight bold))
      ;;                          ("REVIEW"    . (:foreground "yellow" :weight bold))
      ;;                          ("REJECTED"  . (:foreground "lightblue" :weight bold))
      ;;                          ("DONE"      . org-done)
      ;;                          ("ARCHIVED"  . (:foreground "lightblue" :weight bold)))
      )

;; enable visual-line-mode (long lines soft wrap) for org-mode files
(add-hook 'org-mode-hook (lambda () (visual-line-mode t)))

;; agenda files for org-mode
(setq org-agenda-files (file-expand-wildcards "~/.org/*.org"))

;; ;; prevents org-mode for using Shift-(cursors), so windmode can use them
;; not needed if org-replace-disputed-keys is set
;; (setq org-CUA-compatible t)

;; ;; allow reassign S-cursors in org-mode
;; not needed if org-replace-disputed-keys is set
;; (setq org-support-shift-select 'always)

;; ;; make windmove work in org-mode
;; not needed if org-replace-disputed-keys is set
;; (add-hook 'org-shiftup-final-hook 'windmove-up)
;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
;; (add-hook 'org-shiftright-final-hook 'windmove-right)

;; org-mode lists end when \n is found, this prevents bug:
;; http://lists.gnu.org/archive/html/emacs-orgmode/2009-03/msg00582.html
(setq org-empty-line-terminates-plain-lists t)

;; force relative links in org-mode
(setq org-link-file-path-type 'relative)

;; fontify code blocks as their source language
(setq org-src-fontify-natively t)

;; hide asterisks and indent headers in org-mode
(setq org-indent-indentation-per-level 1
      org-startup-indented t)

;; M-RET opens a new headline without spliting the current line
;;(setq org-M-RET-may-split-line '((default . nil)))

;; disable html export validation link
(setq org-html-validation-link nil)

;; use TAB to spread headers in outline-mode, like org-mode
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
            (define-key outline-minor-mode-map [(shift tab)] 'org-global-cycle)))
(add-hook 'outline-mode-hook
          (lambda ()
            (define-key outline-mode-map [(tab)] 'org-cycle)
            (define-key outline-mode-map [(shift tab)] 'org-global-cycle)))

;; enable hide-show for nxml
(require 'sgml-mode)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))
(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; additional tags that are nxml-mode sections for outlining: C-c C-o C-(d|s)
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq nxml-section-element-name-regexp
                  (concat nxml-section-element-name-regexp "\\|widget")
                  nxml-heading-element-name-regexp
                  (concat nxml-heading-element-name-regexp "\\|property"))))

;; printing settings
(setq ps-paper-type 'a4
      ps-header-lines 1
      ps-n-up-printing 1
      ps-n-up-border-p nil)

;; landscape and portrait font sizes for printing
;; 8.5 default, 10 max for no line wrapping in org-mode
(setq ps-font-size '(7 . 10))

;; default browser is firefox and new pages are opened in tabs
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-firefox-new-window-is-tab 't)

;; disable security checks again TLS certificates
(setq network-security-level 'low)

;; place data about secure network connections in the cache directory
(setq nsm-settings-file (expand-file-name "network-security.data" user-emacs-cache-directory))


;;--------------------------------------------------------------------
;;  useful little functions
;;--------------------------------------------------------------------

;; if backups are enabled, store them in ~/.emacs.d/cache/backup if it exists.
;; if not, a single backup file will be kept in ~/.emacs.d/cache/%backup%~
(defun make-backup-file-name (file-name)
  (let ((dir (expand-file-name "backup/" user-emacs-cache-directory)))
    (if (file-directory-p dir)
        (concat (expand-file-name dir)
                (dired-replace-in-string "/" "|" file-name))
      (concat (expand-file-name "%backup%~" user-emacs-cache-directory)))))

;; insert a path into the buffer with completion
(defun insert-path ()
  "Insert a path into the buffer with completion"
  (interactive)
  (insert (expand-file-name (read-file-name "Path: "))))

;; insert timestamp at point as 2002-10-11 20:05
(defun insert-timestamp ()
  "Insert date at point with the format yyyy-mm-dd hh:mm"
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

;; insert date at point as 2002-10-11
(defun insert-date ()
  "Insert date at point with the format yyyy-mm-dd"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; remove ^M's, for files with mixed dos/unix line endings
(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; filter region with perltidy
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

;; filter function with perltidy
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))

;; increment number at cursor
(defun increment-number-at-point (&optional amount)
  "Increment the number under point by `amount'"
  (interactive "p")
  (let ((num (number-at-point)))
    (when (numberp num)
      (let ((newnum (+ num amount))
            (p (point)))
        (save-excursion
          (skip-chars-backward "-.0123456789")
          (delete-region (point) (+ (point) (length (number-to-string num))))
          (insert (number-to-string newnum)))
        (goto-char p)))))

;; toggle between horizontal and vertical window splitting (Jeff Dwork)
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))
    (message "There should be exactly 2 windows on the frame to toggle split mode")))

;; swap buffers shown between two windows (Thomas Bellman)
(defun swap-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; run next-error followed by recenter
(defun next-error-recenter (&optional arg reset)
  "Run next-error and then a recenter of the window."
  (interactive "P")
  (next-error arg reset)
  (recenter))

;; display ascii characters table (Rick Bielawski)
(defun ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (save-excursion
    (let ((i -1))
      (insert "ASCII characters 0 thru 127.\n\n")
      (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
      (while (< i 31)
        (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                        (setq i (+ 1  i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)))
        (setq i (- i 96))))))

;; the opposite to fill-paragraph (Stefan Monnier)
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; replace M-q for this functions that alternates between fill and unfill (Artur Malabarba)
(defun fill-or-unfill-paragraph ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'fill-or-unfill-paragraph)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))
(global-set-key [remap fill-paragraph] #'fill-or-unfill-paragraph)

;; open huge files as read-only (Trey Jackson)
(defun my/find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) 10000000)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (message "Buffer is set to read-only because it is large. Undo also disabled.")
    ;; (set-background-color "light yellow")
    ))
(add-hook 'find-file-hooks 'my/find-file-check-make-large-file-read-only-hook)

;; eval last lisp expression and replace it with its value (Nathaniel Flath)
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; open a shell in the current directory (Nathaniel Flath)
(defun shell-current-directory ()
  "Opens a shell in the current directory"
  (interactive)
  (shell (concat "shell-" default-directory "-shell" )))

;; force vc-dir to start in the root path (fix vc for git)
;; http://www.emacswiki.org/emacs-en/VcTopDirectory
(defadvice vc-dir-prepare-status-buffer
    (before my-vcs-goto-top-directory activate compile)
  (let* ((backend (ad-get-arg 2))
         (vcs-dir (ad-get-arg 1))
         (vcs-top-dir (vc-call-backend backend 'responsible-p vcs-dir)))
    (when (stringp vcs-top-dir)
      (ad-set-arg 1 vcs-top-dir))))

;; rename current buffer and the file it is visiting (rejeep)
(defun rename-file-and-buffer ()
  "Rename current buffer and the file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

;; close all dired buffers
(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

;; abort the minibuffer when using the mouse (Trey Jackson)
(defun my/stop-using-minibuffer ()
  "Kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'my/stop-using-minibuffer)

;; in ruby-mode, # key expands to #{} when typed inside a double quoted string
;; http://blog.senny.ch/blog/2012/10/06/emacs-tidbits-for-ruby-developers/
(defun my/ruby-interpolate ()
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))
(eval-after-load 'ruby-mode
  '(progn (define-key ruby-mode-map (kbd "#") 'my/ruby-interpolate)))

;; colorize ansi color sequences
;; really slow! better remove ANSI sequences with ansi2text in Debian
;; package kbtin
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

;; ediff two files marked in dired with the key 'e'
;; https://oremacs.com/2017/03/18/dired-ediff/
(defun my/dired-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))
(define-key dired-mode-map "e" 'my/dired-ediff-files)

;; report in the minibuffer the sum of every hour amount in the region, being
;; them in the form 1h or 2.3h
(defun my/sum-hours (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let (nums total)
      (while (re-search-forward "\\b[0-9]+\\(\\.[0-9]+\\)*h\\b" end t)
        (push (string-to-number (match-string-no-properties 0))
              nums))
      (setq total (apply #'+ nums))
      (message "%s" total)
      total)))


;;--------------------------------------------------------------------
;;  use ediff instead of emerge, useful for git mergetool -t emerge
;;--------------------------------------------------------------------
;;
;; Theodore Tso  http://kerneltrap.org/mailarchive/git/2007/7/2/250526
;;
;; use-ediff-instead.el
;;
;; This emacs lisp snippet should be placed in your .emacs.el file in
;; order to use the ediff package instead of emerge for git-mergetool.
;; Ediff has more whiz-bang features, but unfortunately it doesn't
;; integrate well with shell scripts that try to invoke ediff from an
;; emacs shell invocation.  This script tries to address these problems.

(defun ediff-write-merge-buffer ()
  (let ((file ediff-merge-store-file))
    (set-buffer ediff-buffer-C)
    (write-region (point-min) (point-max) file)
    (message "Merge buffer saved in: %s" file)
    (set-buffer-modified-p nil)
    (sit-for 1)))

(defun ediff-abort ()
  "Abort the ediff session without a non-zero exit status"
  (interactive)
  (kill-emacs 1))

(defun ediff-setup-abort ()
  (define-key ediff-mode-map "x" 'ediff-abort))

(defun emerge-files-command ()
  (let ((file-a (nth 0 command-line-args-left))
        (file-b (nth 1 command-line-args-left))
        (file-out (nth 2 command-line-args-left)))
    (setq command-line-args-left (nthcdr 3 command-line-args-left))
    (setq ediff-quit-hook 'kill-emacs
          ediff-quit-merge-hook 'ediff-write-merge-buffer
          ediff-keymap-setup-hook 'ediff-setup-abort)
    (ediff-merge-files file-a file-b  nil file-out)))

(defun emerge-files-with-ancestor-command ()
  (let (file-a file-b file-anc file-out)
    ;; check for a -a flag, for filemerge compatibility
    (if (string= (car command-line-args-left) "-a")
        ;; arguments are "-a ancestor file-a file-b file-out"
        (progn
          (setq file-a (nth 2 command-line-args-left))
          (setq file-b (nth 3 command-line-args-left))
          (setq file-anc (nth 1 command-line-args-left))
          (setq file-out (nth 4 command-line-args-left))
          (setq command-line-args-left (nthcdr 5 command-line-args-left)))
      ;; arguments are "file-a file-b ancestor file-out"
      (setq file-a (nth 0 command-line-args-left))
      (setq file-b (nth 1 command-line-args-left))
      (setq file-anc (nth 2 command-line-args-left))
      (setq file-out (nth 3 command-line-args-left))
      (setq command-line-args-left (nthcdr 4 command-line-args-left)))
    (setq ediff-quit-hook 'kill-emacs
          ediff-quit-merge-hook 'ediff-write-merge-buffer
          ediff-keymap-setup-hook 'ediff-setup-abort)
    (ediff-merge-files-with-ancestor file-a file-b file-anc nil file-out)))


;;--------------------------------------------------------------------
;;  simple key shortcuts (shouldn't overwrite any standard key chord)
;;--------------------------------------------------------------------

;; M-g for goto-line
(global-set-key (kbd "M-g") 'goto-line)

;; f5 to open a new frame (C-x 5 2)
(global-set-key (kbd "<f5>") 'make-frame-command)

;; f6 to swap between horizontal and vertical windows splitting
(global-set-key (kbd "<f6>") 'toggle-window-split)

;; shift-f6 to swap two windows' contents
(global-set-key (kbd "S-<f6>") 'swap-windows)

;; resize windows with S-C-<cursorkeys>
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; M-_ to complete words
;; another key press cicles through the matching words
(global-set-key (kbd "M-_") 'hippie-expand)

;; C-M-_ for dabbrev-completion
(global-set-key (kbd "C-M-_") 'dabbrev-completion)

;; f12 for next-error-recenter
(global-set-key (kbd "<f12>") 'next-error-recenter)

;; disable insert key
(global-set-key (kbd "<insert>")
                (lambda () (interactive)
                  (message "Insert key disabled, use M-x overwrite-mode")))

;; same as C-u M-x join-line: join next line to this and fix up whitespace
(global-set-key (kbd "C-c j") (lambda () (interactive) (join-line 4)))

;; M-; comment-or-uncomment-region (overwites comment-dwim keybinding)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)

;; S-F12 for ff-find-other-file
(global-set-key (kbd "S-<f12>") 'ff-find-other-file)

;; 'C-c i' to insert-date
(global-set-key (kbd "C-c i") 'insert-date)

;; 'C-c I' to insert-timestamp
(global-set-key (kbd "C-c I") 'insert-timestamp)

;; C-c e for eval and replace last sexp
(global-set-key (kbd "C-c e") 'eval-and-replace)


;;--------------------------------------------------------------------
;;  notes and reminders
;;--------------------------------------------------------------------
;;
;; - recompile .elc files after major emacs version change:
;;   (byte-recompile-directory package-user-dir nil 'force)
;;
;; - interactive functions to find or set configs:
;;     describe-variable, set-variable, describe-function,
;;     describe-key, find-library, find-function, find-variable
;;
;; - which variable holds given value: apropos-value
;;
;; - file local variables
;;   - at first first line: -*- mode: c; coding: utf-8; c-file-style: "gnu"; -*-
;;   - anywhere (remove inter spaces)
;;     <!--  L o c a l   V a r i a b l e s :  -->
;;     <!--  m o d e :   x m l                -->
;;     <!--  c o d i n g :   u t f - 8        -->
;;     <!--  E n d :                          -->
;;   - info:File Variables
;;   - emacswiki:FileLocalVariables
;;
;; - directory (recursive) local variables
;;   - M-x add-dir-local-variable
;;   - .dir-locals.el  ( (nil (general . settings)) (mode1 (settings . 1)) )
;;   - example, open .h in C++ mode: ((c-mode . ((mode . c++))))
;;
;; - macros
;;   - save: M-x name-last-kbd-macro  M-x insert-kbd-macro
;;   - active older defined: M-x kmacro-cycle-ring-next
;;   - active newer defined: M-x kmacro-cycle-ring-previous
;;
;; - emacs lisp interactive prompt (inferior emacs lisp mode): ielm
;;
;; - find out which packages require the given one. evaluate this sexp
;;   adjusting cl as needed:
;;   (progn (require 'loadhist) (file-dependents (feature-file 'cl)))
;;
;; - show differences between a buffer and its saved file: diff-buffer-with-file
;;
;; - copy the buffer for different outline/narrow: M-x clone-indirect-buffer
;;
;; - change encoding/newlines: set-buffer-file-coding-system (unix, utf-8-unix)
;;
;; - insert char by unicode id: insert-char (<24.3: ucs-insert ID)
;;   examples: #10r243, #xF3, EURO SIGN
;;
;; - describe character at point: describe-char
;;
;; - decode html entities: w3m-decode-entities-string (non-interactive)
;;
;; - recenter window: C-l; move cursor around window: M-r
;;
;; - column editing mode (like Scintilla or Visual Studio)
;;   - emacs >= 24.4
;;     - standard: C-x SPC ; movement ; supr ; C-t to write
;;     - cua not needed: M-x cua-rectangle-mark-mode ; movement ; supr ; write
;;   - emacs < 24.4
;;     - cua-mode enabled: C-RET ; movement ; supr ; write
;;
;; - swap position and mark: C-x C-x; go to previous marks: C-x C-SPC
;;
;; - remove spaces and newlines around: C-u-1 M-SPC
;;
;; - remove region without saving it to the kill ring: M-x delete-region
;;
;; - write region into another file without visiting it: write-region
;;
;; - tramp
;;   - root:   /su::/etc/passwd  or  /sudo::/etc/passwd
;;   - ssh:    /ssh:user@host:/path/filename
;;   - chain:  /ssh:user@remotehost|sudo:[user@]remotehost:/etc/passwd
;;
;; - keep paragraphs in the column limit:
;;   - hard:         fill-paragraph (M-q); inverse: unfill-paragraph (or M-q again)
;;   - hard:         refill-mode
;;   - soft:         visual-line-mode (>=23), longlines-mode (>=22)
;;   - set limit:    set-fill-column (default 70)
;;   - indentation:  set-justification-(full|left|right|center|none)
;;
;; - html-mode
;;   - jump to next or previous matching tag: C-c right  or  C-c left
;;   - toggle hide tags (kind of html rendering): C-c TAB
;;
;; - web-mode
;;   - jump to matching tag: C-c C-n
;;   - fold tag contents: C-c C-f
;;
;; - deactivate auto indentation: c-toggle-electric-state
;;
;; - show line numbers like vim: (global-)display-line-numbers-mode (emacs26)
;;   or linum-mode (emacs23) or setnu-mode (older versions)
;;
;; - ivy / swiper / counsel
;;   - git grep: C-c p
;;   - ripgrep: C-c k
;;   - open selection: RET  or  C-m  (without closing ivy: C-M-m)
;;   - open without closing and next/prev, useful for counsel-rg: C-M-n  or C-M-p
;;   - resume last ivy session  after close: C-c r
;;   - display actions: M-o  (without closing ivy: C-M-o)
;;   - display hydra actions: C-o
;;   - follow directory instead of opening it in dired: C-j  or  right
;;   - insert the current candidate into the minibuffer for edition: M-i
;;   - search for the word under the cursor: M-n
;;   - add substrings at the point in buffer to the search like C-w in isearch: M-j
;;   - force select exactly what has been input (ex: create new files): C-M-j
;;   - display completion keys to select faster than moving and RET: C-'
;;   - remove input: S-SPACE
;;   - show current search in an occur buffer: C-c C-o
;;   - launch replacement process for currently searched string: M-q
;;   - change to parent directory (if completion empty): BACKSPACE
;;   - change to home directory: ~
;;   - change to root directory: //
;;   - show recent opened files: C-c f
;;   - show files in current git repository: C-c g
;;   - show files recursively from current directory: C-c l  (counsel-file-jump)
;;   - show kill ring history: M-y
;;   - show list of current key bindings: M-x counsel-descbinds
;;   - show list of themes: M-x counsel-load-theme
;;   - replace in files
;;     - search with ivy, ie: counsel-git-grep, counsel-rg, counsel-ag, ...
;;     - open occur buffer with C-c C-o (ivy-occur)
;;     - toggle occur buffer to writable: w (ivy-wgrep-change-to-wgrep-mode)
;;     - edit buffer, apply with C-c C-c (wgrep-finish-edit) or reject with C-c C-k (wgrep-abort-changes)
;;     - save all buffers: C-x s  (followed by ! when asked)
;;
;; - tags
;;   - definition: M-.    global:gtags-find-tag   etags:find-tag  next: C-u M-.
;;   - reference:  C-.    global:gtags-find-rtag  etags:unavailable!
;;   - return:     M-,    global:gtags-pop-stack  etags:pop-tag-mark
;;   - completing: M-TAB  global:find out!        etags:complete-tag
;;   - gtags:
;;       create:  gtags
;;       update:  global -u
;;   - etags:
;;       rm TAGS; find -type f -name '*.[hc]' | xargs etags -a
;;
;; - semantic bovinator
;;   - activate: semantic-mode
;;   - jump to definition: semantic-ia-fast-jump
;;
;; - repeat last command: C-x z  ex: C-u M-. (find next tag) C-x z z z z
;;
;; - open .h/.c file or #include at point (S-F12): (C-u) ff-find-other-file
;;
;; - [CC] add a new C style based on the file: M-x c-guess
;;
;; - [CC] go to begin/end of fuctions: C-M-a / C-M-e
;;
;; - balanced expressions (parenthesis, brackets, etc)
;;   - move forward-sexp: C-M-f
;;   - move backward-sexp: C-M-b
;;   - kill-sexp: C-M-k
;;   - put mark at the end mark-sexp: C-M-SPC
;;
;; - vertically align region based on regular expression: align-regexp
;;   http://irreal.org/blog/?p=169
;;
;; - sort lines by (optionally) rectangular region: sort-columns
;;
;; - helper to create regular expressions: re-builder
;;
;; - actions when replacing with M-%:
;;   - replace and edit:  C-w
;;   - edit:              C-r
;;
;; - append following kill to kill ring, so yank pastes all: C-M-w
;;
;; - edit read-only files: read-only-mode (<24.3: toggle-read-only)
;;
;; - emacs-lock-mode is a minor mode to protect a buffer from being closed
;;
;; - insert \n without moving the cursor: C-o (open-line)
;;
;; - increment/decrement font size: C-x C-+ / C-x C-- (>=23)
;;
;; - highlight line under cursor: hl-line-mode or global-hl-line-mode
;;
;; - view a big buffer sequentially in 2 windows: follow-mode and C-x 3
;;
;; - scroll at the same time two unrelated windows: scroll-all-mode
;;
;; - dired
;;   - w: copy filename to kill ring
;;   - C-u w: copy relative path to kill ring
;;   - C-u 0 w: copy absolute path to kill ring
;;   - t: mark all files
;;   - Q: replace in marked files
;;   - M-x wdired-change-to-wdired-mode: change to writable buffer to rename files
;;   - M-x virtual-dired: use a buffer containing filenames as a dired view
;;
;; - grep or ag
;;   - automatically show matching buffer/line under cursor: next-error-follow-minor-mode
;;   - edit matches: wgrep-change-to-wgrep-mode
;;
;; - occur
;;   - edit with 'e' in occur buffer to modify original buffers
;;   - C-c C-f  automatically advance the source buffer when moving cursor in occur buffer
;;   - C-c C-c  cancel C-c C-f
;;
;; - show previously shown buffers in the window: switch-to-(prev|next)-buffer
;;
;; - calc
;;   - launch
;;     - Window at the bottom of the frame: M-x calc
;;     - Whole frame: M-x full-calc
;;     - Whole frame with buttons for mathematical functions: M-x full-calc-keypad
;;   - usage in calc mode
;;     - toggle showing stack window (calc-trail-display): t d
;;     - change display radix, ex: d r 16
;;     - insert number in the current radix when it needs letters: #deadbeef
;;     - insert number in a diferent radix, ex: 16#deadbeef
;;     - algebraic input: '
;;     - negate: n
;;     - swap last two values: TAB
;;     - empty stack: C-x * 0
;;   - from any buffer prefixed by C-x *
;;     - quick algebraic calculation: q
;;     - embedded formula, or return to editing buffer: e
;;     - embedded word: w
;;     - grab region into calc: g
;;   - both in calc mode or in another buffer with prefix
;;     - help: ? ?
;;     - yank to buffer: y
;;
;; - org-mode
;;   - hierarchy
;;     - show/hide element: TAB
;;     - show/hide element and its children: S-TAB
;;     - new heading: M-RET
;;     - change deep level: M-LEFT, M-RIGHT
;;     - move: M-UP, M-DOWN
;;     - convert heading to list item and reverse: 'C-c -', 'C-c *'
;;     - toggle archive in the same position: C-c C-x a
;;     - archive in the same tree: C-c C-x A
;;   - marks
;;     - to do:    'M--', 'M-+'
;;     - priority: 'M-p', 'M-n', 'C-c ,'
;;     - tags:     'C-c C-c'
;;   - checkboxes for lists
;;     - [X] toggle check:     C-c C-c
;;     - [ ] add or remove:    C-u C-c C-c
;;     - [-] toggle half done: C-u C-u C-c C-c
;;     - add new list entry with checkbox: M-S-RET
;;   - dates
;;     - modify:    M--  M-+  M-p  M-n  C-c .
;;     - timestamp: C-c .
;;     - schedule:  C-c C-s
;;     - deadline:  C-c C-d
;;   - links
;;     - anchor:  <<<foo>>>
;;     - link:    [[expr][title]], [[*expr][title]], [[file:/path][title]]
;;     - visit:   C-c C-o
;;     - return:  C-c &
;;     - link to a buffer position: org-store-link, org-insert-link
;;   - tables
;;     - copy down: C-S-RET
;;     - convert a region to a table: org-table-convert-region
;;     - recalculate: column[C-c C-c] line[C-c *] table[C-u C-c *] iterate[C-u C-u C-c *] all[C-u C-u C-c C-c]
;;     - formula editor: C-c '
;;   - literal examples
;;     - #+BEGIN_SRC ruby  ..  #+END_SRC
;;     - shortcut: write <e or <s and press TAB
;;     - edit: C-c '
;;   - export dispatcher C-c C-e
;;     - latex: l o
;;       - insert \pagebreak[1] between headers if there are lots of them without any content
;;       - open pdf with evince instead of evince-previewer:
;;         echo "evince:application/pdf" | sudo tee -a /etc/mailcap.order && sudo update-mime
;;   - scheduling
;;     - add effort property to an item: C-c C-x e  (org-set-effort)
;;     - configure and display table, refresh with C-c C-c over the BEGIN line:
;;
;;       :PROPERTIES:
;;       :COLUMNS:  %80ITEM(Task) %10Effort(Effort){:} %10TODO(Status) %10TAGS(Tags)
;;       :END:
;;
;;       #+BEGIN: columnview :hlines nil :id local :indent t :skip-empty-rows t
;;       #+END:
;;
;; - M-x term (like shell or eshell but with full curses support)
;;   - C-c C-k  term-char-mode: every char but C-c is sent to the terminal (default)
;;   - C-c C-j  term-line-mode: only full lines are sent, any other keys are like normal emacs
;;
;; - draw diagrams with picture-mode
;;   - C-c C-c  exits picture-mode clearing whitespace
;;   - C-c C-r  draw rectangle between mark and point
;;
;; - load library without failure if it doesn't exists
;;   - .emacs:  (require 'graphviz-dot-mode nil t)
;;   - some-library.el (add if not present): (provide 'graphviz-dot-mode)
;;
;; - show how long it tooks emacs to start: emacs-init-time
;;
;; - snip from my .Xresources to enable full font hinting (use Gnome Tweak Tool
;;   to enable it in gnome-terminal for nw mode):
;;
;;     Xft.antialias:  1
;;     Xft.hinting:    1
;;     Xft.hintstyle:  hintfull
;;     Xft.lcdfilter:  lcddefault
;;     Xft.rgba:       rgb
;;
;; - Uncommon elisp tutorials:
;;   - http://ergoemacs.org/emacs/elisp_common_functions.html
;;


;;--------------------------------------------------------------------
;;  temporaly saved macros
;;--------------------------------------------------------------------


;;--------------------------------------------------------------------
;;  automatic customizations will be placed here to be moved later on
;;--------------------------------------------------------------------
