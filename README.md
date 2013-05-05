# stripe-buffer mode

Use different background colors for even and odd lines.

With the help of library `hl-line-mode` yet another color can be used
for the current line.

![screenshot](https://github.com/sabof/stripe-buffer/raw/master/screenshot.png)

Based on the [original](http://www.emacswiki.org/emacs/StripeBuffer)
`stripe-buffer.el` by Andy Steward.

## Usage:

### Common case:

    (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)

### Add stripes in list-style modes (ex. dired-mode)

As above, or you can use the following to get a horizontal line instead of a
cursor. It uses the `stripe-hl-line` face, which you might wish to customize.

    (add-hook 'dired-mode-hook 'stripe-listify-buffer)

### Add stripes to tables

You might want to have stripes only for tables. Whether a line will be
considered a "table line" is determined by `stripe-in-table-regex`. The default value supports org-mode and table.el tables, as well as tables printed by mysql.

    (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)

## Customization:

### Faces

`stripe-highlight` -- color of stripes

`stripe-hl-line` -- color for hl-line, when using `stripe-listify-buffer`

### Variables

`stripe-height` -- height of the stripes

`stripe-in-table-regex` -- Regex for determining whether a line is part of a table. Used in `stripe-table-mode`
