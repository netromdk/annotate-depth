[![MELPA](http://melpa.org/packages/annotate-depth-badge.svg)](http://melpa.org/#/annotate-depth)

# Annotate Depth mode [Emacs]

`annotate-depth-mode` annotates buffer if indentation depth is beyond threshold (see `annotate-depth-threshold` which defaults to 5). An idle timer is started when entering the mode and disabled when exiting it. The face `annotate-depth-face` is applied at indentation level and to end-of-line for each line on or beyond threshold.

Usage:
```elisp
(add-hook 'prog-mode-hook 'annotate-depth-mode)
```
