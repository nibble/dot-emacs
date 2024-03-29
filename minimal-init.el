;; Minimal emacs init file with better defaults for quick edit
;; sessions in slow computers without emacs-server.
;;
;; Example of launch script:
;;
;;   #!/bin/sh
;;   exec emacs -Q -nw -l ~/.emacs.d/minimal-init.el $*

;; Set eln-cache location inside ~/.emacs.d/cache
(when (boundp 'native-comp-eln-load-path)
  (setq user-emacs-cache-directory (expand-file-name "cache/" user-emacs-directory))
  (if (>= emacs-major-version 29)
      (startup-redirect-eln-cache (expand-file-name "eln-cache/" user-emacs-cache-directory))
    (setcar native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-cache-directory))))

;; Usability settings

(setq-default indent-tabs-mode nil)

(setq make-backup-files nil
      auto-save-default nil
      shift-select-mode nil
      transient-mark-mode nil
      visible-bell t
      initial-scratch-message nil
      global-eldoc-mode nil
      dabbrev-case-fold-search nil
      dabbrev-case-replace nil)

(prefer-coding-system 'utf-8-unix)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)
(show-paren-mode 1)
(menu-bar-mode -1)
(when (window-system)
  (ignore-errors (tool-bar-mode -1)))

(windmove-default-keybindings)

(global-set-key [?\M-\;] 'comment-or-uncomment-region)
(global-set-key [?\M-_] 'dabbrev-expand)

;; Workaround to wrong colors when running inside tmux, not needed for
;; emacs >= 26 or when a color theme is used
(when (and (< emacs-major-version 26) (not (display-graphic-p)))
  (setq frame-background-mode 'light)
  (frame-set-background-mode nil))
