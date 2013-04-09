;;; stripe-buffer.el --- Use a different background for even and odd lines

;; Copyright (C) 2008-2009  Andy Stewart
;; Copyright (C) 2012-2013  sabof

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: sabof <esabof@gmail.com>
;; URL: https://github.com/sabof/stripe-buffer
;; Version: 0.1

;;; Commentary:

;; Use different background colors for even and odd lines.  With the
;; help of library `hl-line' yet another color can be used for the
;; current line.

;; The project is hosted at https://github.com/sabof/stripe-buffer

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(defgroup stripe-buffer nil
  "Use different background for even and odd lines."
  :group 'wp)

(defface stripe-highlight
    '((((class color) (background dark))
       (:background "#444444"))
      (((class color) (background light))
       (:background "#CCCCCC")))
  "Face for stripes."
  :group 'stripe-buffer)

(defface stripe-hl-line
    `((t
       :background ,(face-attribute 'default :foreground)
       :foreground ,(face-attribute 'default :background)))
  "Bold face for highlighting the current line in Hl-Line mode."
  :group 'stripe-buffer)

(defcustom stripe-height 1
  "Height of stripes."
  :group 'stripe-buffer
  :type 'integer)

(defvar stripe-highlight-face 'stripe-highlight)
(defvar stripe-highlight-overlays nil)
(make-variable-buffer-local 'stripe-highlight-overlays)
(defvar stripe-buffer-listified nil)
(make-variable-buffer-local 'stripe-buffer-listified)

(defun stripe-buffer-clear-stripes ()
  "Clear stripe overlays in current buffer."
  (mapc 'delete-overlay stripe-highlight-overlays)
  (setq stripe-highlight-overlays nil))

(defun* stripe-buffer-jit-lock (&optional beginning end)
  (let* (( interval
           (* 2 stripe-height))
         ( old-overlays
           (prog1 stripe-highlight-overlays
             (setq stripe-highlight-overlays nil)))
         ( get-overlay-create
           (lambda (start end)
             (let ((old-overlay (pop old-overlays)))
               (if old-overlay
                   (progn
                     (move-overlay old-overlay start end)
                     old-overlay)
                   (make-overlay start end)))))
         ( draw-stripe
           (lambda (height)
             (let (( overlay
                     (funcall
                      get-overlay-create
                      (point)
                      (progn
                        (forward-line height)
                        (point)))))
               (overlay-put overlay 'face stripe-highlight-face)
               (overlay-put overlay 'is-stripe t)
               (push overlay stripe-highlight-overlays))))
         ( goto-start-pos
           (lambda ()
             (let (( start-offset (mod (line-number-at-pos) interval)))
               (if (< start-offset stripe-height) ; in first part
                   (progn
                     (forward-line (- stripe-height start-offset))
                     (funcall draw-stripe stripe-height))
                   (funcall draw-stripe (- interval start-offset))
                   )))))
    (save-excursion
      (cl-dolist (region (es-buffer-visible-regions-merged))
        (goto-char (car region))
        (funcall goto-start-pos)
        (while (< (point) (cdr region))
          (forward-line stripe-height)
          (funcall draw-stripe stripe-height)
          ))
      (mapc 'delete-overlay old-overlays))))

(defun stripe-org-table-jit-lock (&optional beginning end)
  "Originally made for org-mode tables, but can be used on any
table with org or table.el syntax. Can be called interactively
ex. while viewing the output from MySql select."
  (interactive)
  (let ((last-line (line-number-at-pos (point-max)))
        (in-table-regex "^[ 	]*\\([|+].+[|+]\\) *$"))
    (stripe-buffer-clear-stripes)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp in-table-regex nil t)
        (dotimes (iter (1- stripe-height))
          (when (save-excursion
                  (forward-line)
                  (string-match-p
                   in-table-regex
                   (buffer-substring
                    (line-beginning-position)
                    (line-end-position))))
            (forward-line)))
        (let ((overlay (make-overlay (match-beginning 1) (line-end-position))))
          (overlay-put overlay 'face stripe-highlight-face)
          (push overlay stripe-highlight-overlays)
          (forward-line (1+ stripe-height)))))))

;;; Interface

(define-minor-mode stripe-buffer-mode
    "Stripe buffer mode"
  nil nil nil
  (if stripe-buffer-mode
      (progn
        (add-hook 'post-command-hook 'stripe-buffer-jit-lock nil t)
        (add-hook 'window-scroll-functions 'stripe-buffer-jit-lock nil t)
        (add-hook 'change-major-mode-hook 'stripe-buffer-clear-stripes nil t)
        (add-hook 'window-configuration-change-hook 'stripe-buffer-jit-lock nil t)
        (stripe-buffer-jit-lock))
      (progn
        (remove-hook 'post-command-hook 'stripe-buffer-jit-lock t)
        (remove-hook 'window-scroll-functions 'stripe-buffer-jit-lock t)
        (remove-hook 'change-major-mode-hook 'stripe-buffer-clear-stripes t)
        (remove-hook 'window-configuration-change-hook 'stripe-buffer-jit-lock t)
        (stripe-buffer-clear-stripes)
        )))

(defun stripe-org-tables-enable ()
  "Add stripes to tables in org mode."
  (interactive)
  (jit-lock-register 'stripe-org-table-jit-lock))

(defalias 'org-table-stripes-enable 'stripe-org-tables-enable
  "Backward compatibility")

(defun turn-on-stripe-buffer-mode ()
  "Turn on `stripe-buffer-mode'."
  (interactive)
  (hl-line-mode 1))

(defun stripe-listify-buffer ()
  "Turn on `stripe-buffer-mode' and `hl-line-mode'."
  (interactive)
  (setq cursor-type t)
  (stripe-buffer-mode 1)
  (setq-local face-remapping-alist
              `((hl-line stripe-hl-line)))
  (hl-line-mode 1))

(eval-after-load 'hl-line
  `(progn

     (defadvice hl-line-highlight (after stripe-set-priority activate)
       (when stripe-buffer-mode
         (overlay-put hl-line-overlay 'priority 10)))

     ))

(defun stripe-wdired-enable-cursor ()
  (when (and stripe-buffer-mode stripe-buffer-listified)
    (setq cursor-type t)))

(add-hook 'wdired-mode-hook 'stripe-wdired-enable-cursor)

(defadvice wdired-finish-edit
    (before stripe-hide-cursor activate)
  (when (and stripe-buffer-mode stripe-buffer-listified)
    (setq cursor-type nil)))

(provide 'stripe-buffer)
;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-indent-function: common-lisp-indent-function
;; lisp-backquote-indentation: t
;; End:
;;; stripe-buffer.el ends here
