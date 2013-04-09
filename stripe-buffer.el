;;; stripe-buffer.el --- Use a different background for even and odd lines

;; Copyright (C) 2008-2009  Andy Stewart
;; Copyright (C) 2012-2013  sabof

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: sabof
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

(defcustom stripe-max-buffer-size 0
  "Don't add stripes if buffer has more characters than this.
This is useful, since a large number of overlays can make editing
slow.  When the value is 0 stripes are added regardless of the
number of characters.  50000 is probably a good value."
  :group 'stripe-buffer
  :type 'integer)

(defcustom stripe-height 1
  "Height of stripes."
  :group 'stripe-buffer
  :type 'integer)

(defvar stripe-highlight-face 'stripe-highlight)
(defvar stripe-highlight-overlays nil)
(make-variable-buffer-local 'stripe-highlight-overlays)

(defun stripe-buffer-clear-stripes ()
  "Clear stripe overlays in current buffer."
  (mapc 'delete-overlay stripe-highlight-overlays)
  (setq stripe-highlight-overlays nil))

(defun stripe-buffer-jit-lock (&optional beginning end)
  (stripe-buffer-clear-stripes)
  (unless (and (numberp stripe-max-buffer-size)
               (not (zerop stripe-max-buffer-size))
               (> (point-max) stripe-max-buffer-size))
    (save-excursion
      (goto-char (point-min))
      (forward-line stripe-height)
      (while (not (eobp))
        (let ((overlay (make-overlay
                        (line-beginning-position)
                        (min (1+ (progn
                                   (forward-line
                                    (1- stripe-height))
                                   (line-end-position)))
                             (point-max)))))
          (overlay-put overlay 'face stripe-highlight-face)
          (push overlay stripe-highlight-overlays)
          (forward-line (1+ stripe-height)))))))

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
  (cond (stripe-buffer-mode
         (jit-lock-register 'stripe-buffer-jit-lock)
         (stripe-buffer-jit-lock))
        (t
         (jit-lock-unregister 'stripe-buffer-jit-lock)
         (stripe-buffer-clear-stripes))))

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
  (stripe-buffer-mode 1)
  (hl-line-mode 1))

(eval-after-load 'hl-line
  '(unless (require 'hl-line+ nil t)
     (defadvice hl-line-highlight (after set-priority activate)
       (overlay-put hl-line-overlay 'priority 10))))

(provide 'stripe-buffer)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; stripe-buffer.el ends here
