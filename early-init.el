;;--------------------------  -*- coding: utf-8; mode: emacs-lisp; -*-
;; ~/.emacs.d/early-init.el        Javier Donaire <jdonaire@gmail.com>
;;--------------------------------------------------------------------

(provide 'early-init)

;;--------------------------------------------------------------------
;; Settings needed before initialization
;;--------------------------------------------------------------------

;; Defer garbage collection
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 2000000)))

;; Ensure ~/.emacs.d/cache directory exists
(setq user-emacs-cache-directory (expand-file-name "cache/" user-emacs-directory))
(when (not (file-directory-p user-emacs-cache-directory))
  (make-directory user-emacs-cache-directory nil))

;; Set eln-cache location inside ~/.emacs.d/cache
(when (boundp 'native-comp-eln-load-path)
  (if (>= emacs-major-version 29)
      (startup-redirect-eln-cache (expand-file-name "eln-cache/" user-emacs-cache-directory))
    (setcar native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-cache-directory))))

;; Hide the menu
(menu-bar-mode -1)

;; Hide the tool bar, protected because emacs could be compiled without X
(when (boundp 'tool-bar-mode)
  (ignore-errors (tool-bar-mode -1)))

;; Hide scroll bar
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Default font
(defvar cfg-font-ttf "monospace-10")

;; Default font in windows
(when (eq system-type 'windows-nt) (setq cfg-font-ttf "Consolas 10"))

;; Default font for old emacs versions not using ttf
(defvar cfg-font-x "-misc-fixed-medium-r-normal--*-*-*-*-*-90-iso8859-*")

;; Settings loaded from local-early.el
(load (expand-file-name "local-early.el" user-emacs-directory) t)

;; Example of local-early.el:
;; (setq cfg-font-ttf "inconsolata-10"
;;       cfg-font-x "-*-terminus-medium-r-*-*-*-*-*-*-*-60-iso8859-*")

;; Set selected font
(cond ((>= emacs-major-version 23)
       (add-to-list 'default-frame-alist `(font . ,cfg-font-ttf)))
      ((>= emacs-major-version 22)
       (add-to-list 'default-frame-alist `(font . ,cfg-font-x)))
      ((window-system)
       (set-face-font 'default cfg-font-x)))

;; Premature redisplays can substantially affect startup times and produce ugly
;; flashes of unstyled Emacs
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))
