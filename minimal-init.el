;; Minimal emacs init file with sane (for me) defaults for quick edit
;; sessions in slow computers without emacs-server.
;;
;; Example of launch script:
;;
;;   #!/bin/sh
;;   exec emacs -Q -nw -l ~/.emacs.d/minimal-init.el $*

(setq make-backup-files nil
      auto-save-default nil
      shift-select-mode nil
      transient-mark-mode nil
      visible-bell t
      initial-scratch-message nil
      global-eldoc-mode nil)

(prefer-coding-system 'utf-8-unix)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)
(show-paren-mode 1)
(menu-bar-mode -1)
(when (boundp 'tool-bar-mode)
  (ignore-errors (tool-bar-mode -1)))

(windmove-default-keybindings)

(global-set-key (kbd "M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-_") 'dabbrev-expand)

;; workaround to wrong colors when running inside tmux, not needed in
;; emacs >= 26 or when a color theme is used
(when (and (< emacs-major-version 26) (not (display-graphic-p)))
  (setq frame-background-mode 'light)
  (frame-set-background-mode nil))
