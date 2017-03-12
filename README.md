[![Build Status](https://travis-ci.org/edkolev/evil-lion.svg?branch=master)](https://travis-ci.org/edkolev/evil-lion)

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

Align with `gl MOTION CHAR` or right-align with `gL MOTION CHAR`.
If the align separator is `/` you will be prompted for a regular expression instead of a plain character.

Example, left align `gl`:

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

Example, right align with `gL`:

After pressing `gLip=`
```
one, two, three,
fifteen, sixteen, seventeen
```

will become:

```
one,     two,     three,
fifteen, sixteen, seventeen
```

TODO
----
- passing RET as CHAR should plain call align
