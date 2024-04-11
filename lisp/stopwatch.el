;;; stopwatch.el --- time yo'self -*- lexical-binding: t; -*-

;; Maintainer: asnr <https://github.com/asnr>
;; Version: 0.0.1
;; URL: https://github.com/asnr/stopwatch

;;; Commentary:

;; A stopwatch.

;;; Code:

;(load-file (expand-file-name "stopwatch-mode.el" (file-name-directory load-file-name)))
(load "stopwatch-mode.el")
(load "stopwatch-controller.el")
(load "stopwatch-model.el")
(load "stopwatch-ascii-art-numbers.el")
(load "stopwatch-draw.el")
(load "stopwatch-time-helpers.el")

(defun stopwatch ()
  "Open a stopwatch in a new window."
  (interactive)
  (let* ((stopwatch-buffer (generate-new-buffer "stopwatch"))
         (controller (stopwatch-controller-construct stopwatch-buffer)))
    (stopwatch-controller-start controller)))

(provide 'stopwatch)

;;; stopwatch.el ends here
