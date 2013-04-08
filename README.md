# stripe-buffer mode
Use different background for even and odd lines

![screenshot](https://github.com/sabof/stripe-buffer/raw/master/screenshot.png)

Based on the [original](http://www.emacswiki.org/emacs/StripeBuffer)
`stripe-buffer.el` by Andy Steward.

## Usage:

### Add stripes in dired mode
If you have hl-line+ installed, it will also change the cursor to a highlighted line. You can get hl-line+ [here](http://www.emacswiki.org/emacs-en/download/hl-line%2b.el), or from melpa

    (add-hook 'dired-mode-hook 'stripe-listify-buffer)

### Adds stripes to tables in org-mode

    (add-hook 'org-mode-hook 'stripes-org-table-enable)
