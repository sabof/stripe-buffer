# stripe-buffer mode
Use different background for even and odd lines

Based on stripe-buffer.el by Andy Steward

![screenshot](https://github.com/sabof/stripe-buffer/raw/master/screenshot.png)

## Usage:

### Add stripes in dired mode
If you have hl-line+ installed, it will also change the cursor to a highlighted line

    (add-hook 'dired-mode-hook 'stripe-listify-buffer)

### Adds stripes to tables in org-mode

    (add-hook 'org-mode-hook 'org-table-stripes-enable)