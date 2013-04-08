# stripe-buffer mode

Use different background colors for even and odd lines.

With the help of library `hl-line-mode` yet another color can be used
for the current line.

![screenshot](https://github.com/sabof/stripe-buffer/raw/master/screenshot.png)

Based on the [original](http://www.emacswiki.org/emacs/StripeBuffer)
`stripe-buffer.el` by Andy Steward.

## Usage:

### Add stripes in dired mode

To use different background colors for even and odd lines use:

    (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)

If you also want to use yet another color for the current line use:

    (add-hook 'dired-mode-hook 'stripe-listify-buffer)

### Adds stripes to tables in org-mode

In Org-mode you likely want to add stripes only to tables:

    (add-hook 'org-mode-hook 'stripes-org-table-enable)
