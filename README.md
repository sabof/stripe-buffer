# stripe-buffer mode
Use different background for even and odd lines

Based on stripe-buffer.el by Andy Steward

## Usage:
    M-x stripe-buffer-mode
Toggle stripes in the current buffer
    (add-hook 'org-mode-hook 'org-table-stripes-enable)
Adds stripes to tables in org-mode
    (add-hook 'dired-mode-hook 'stripe-listify-buffer)
    (add-hook 'dired-mode-hook (lambda () (stripe-buffer)
Adds stripes, hides the cursor, and enables hl-line in dired.
hl-line doesn't support custom priorities, so you might want to use hl-line+.
Othewise the line highlighting will be obscured by stripes.