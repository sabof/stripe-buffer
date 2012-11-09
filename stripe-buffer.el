;;; stripe-buffer.el --- Use different background for even and odd lines

;;; Commentary:

;; The project is hosted at https://github.com/sabof/stripe-buffer
;; The latest version, and all the relevant information can be found there.

(require 'cl)

(defface stripe-highlight
    '((((class color) (background dark))
       (:background "#444444"))
      (((class color) (background light))
       (:background "#CCCCCC")))
  "Face for highlighting current overlay."
  :group 'basic-faces)

(defvar stripe-highlight-face 'stripe-highlight
  "The face variable for `stripe-buffer-on'.")

(defvar stripe-highlight-overlays nil
  "The overlays for `stripe-buffer'.")

(make-variable-buffer-local 'stripe-highlight-overlays)

(defvar stripe-max-buffer-size 50000
  "Stripe buffer isn't really suitable for large buffers.
Set to nil, if you want it enabled no matter the size.")

(defvar stripe-height 1)

(defun stripe-buffer-clear-stripes ()
  "Clear stripe overlays in current buffer."
  (mapc 'delete-overlay stripe-highlight-overlays)
  (setq stripe-highlight-overlays nil))

(defun* stripe-buffer-jit-lock (beginning end)
  ;; (stripe-buffer-clear-stripes)
  ;; (return-from stripe-buffer-jit-lock)
  (let ((beginning (save-excursion
                     (goto-char beginning)
                     (line-beginning-position)))
        (end (save-excursion
               (goto-char end)
               (line-end-position))))
    (dolist (overlay (overlays-in beginning end))
      (when (overlay-get overlay 'is-stripe)
        (delete-overlay overlay)))
    ;; (when (and (numberp stripe-max-buffer-size)
    ;;            (> (point-max) stripe-max-buffer-size))
    ;;   (return-from stripe-buffer-jit-lock))
    (save-excursion
      ;; (goto-char (point-min))
      (goto-char beginning)
      (forward-line stripe-height)
      (while (and (not (eobp))
                  (<= (point) end))
        (let ((overlay (make-overlay
                        (line-beginning-position)
                        (min (1+ (progn
                                   (forward-line (1- stripe-height))
                                   (line-end-position)))
                             (point-max)))))
          (overlay-put overlay 'face stripe-highlight-face)
          (overlay-put overlay 'is-stripe t)
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
  (if stripe-buffer-mode
      (jit-lock-register 'stripe-buffer-jit-lock)
      (progn
        (jit-lock-unregister 'stripe-buffer-jit-lock)
        (stripe-buffer-clear-stripes)
        )))

(defun stripe-org-tables-enable ()
  "Add stripes to tables in org mode."
  (jit-lock-register 'stripe-org-table-jit-lock))

(defalias 'org-table-stripes-enable 'stripe-org-tables-enable
  "Backward compatibility")

(defun stripe-listify-buffer ()
  ;; (require 'hl-line)
  (if  (featurep 'hl-line+)
       (progn
         (require 'hl-line+)
         (stripe-buffer-mode 1)
         (setq cursor-type nil)
         (hl-line-mode 1))
       (require 'hl-line+)))

(provide 'stripe-buffer)

;;; stripe-buffer.el ends here