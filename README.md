
evil-lion
=========

Evil align operator, port of vim-lion by Tom McDonald (https://github.com/tommcdo/vim-lion)

Installation
------------

``` emacs-lisp
;; Note: evil-lion is not on melpa yet, this snippet isn't valid
(use-package evil-lion
  :ensure t
  :config
  (evil-lion-install))
```

Usage
-----

Example:

After pressing `glip=` (`gl` is the operator, `ip` text object paragraph, `=` separator)
```
one = 1
three = 3
fifteen = 15
```

will become:

```
one     = 1
three   = 3
fifteen = 15
```

If the align separator is `/` you will be prompted for a regular expression instead of a plain character.

TODO
----
- visual mode
- `gL` in addition to `gl`
- try to make `gl` work as vim-lion's `gl`
- maybe support count
