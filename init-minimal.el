;; Minimal emacs init file with sane (for me) defaults for quick edit
;; sessions in slow computers without emacs-server.
;;
;; Example of launch script:
;;
;;   #!/bin/sh
;;   exec emacs -Q -nw -l ~/.emacs.d/init-minimal.el $*

(setq make-backup-files nil
      shift-select-mode nil
      transient-mark-mode nil
      visible-bell t
      initial-scratch-message nil)

(prefer-coding-system 'utf-8-unix)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)
(menu-bar-mode -1)
(if (boundp 'tool-bar-mode)
    (ignore-errors (tool-bar-mode -1)))

(windmove-default-keybindings)

(global-set-key (kbd "M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-_") 'dabbrev-expand)
