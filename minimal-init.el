;; Minimal emacs init file with better defaults for quick edit
;; sessions in slow computers without emacs-server.
;;
;; Example of launch script:
;;
;;   #!/bin/sh
;;   exec emacs -Q -nw -l ~/.emacs.d/minimal-init.el $*

(setq-default indent-tabs-mode nil
              case-fold-search nil)

(setq make-backup-files nil
      auto-save-default nil
      shift-select-mode nil
      transient-mark-mode nil
      visible-bell t
      initial-scratch-message nil
      global-eldoc-mode nil
      case-replace nil
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

(prefer-coding-system 'utf-8-unix)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)
(show-paren-mode 1)
(menu-bar-mode -1)
(when (window-system)
  (ignore-errors (tool-bar-mode -1)))

(windmove-default-keybindings)

(global-set-key [?\M-;] 'comment-or-uncomment-region)
(global-set-key [?\M-_] 'hippie-expand)

;; workaround to wrong colors when running inside tmux, not needed in
;; emacs >= 26 or when a color theme is used
(when (and (< emacs-major-version 26) (not (display-graphic-p)))
  (setq frame-background-mode 'light)
  (frame-set-background-mode nil))
