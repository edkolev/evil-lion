[![Build Status](https://travis-ci.org/edkolev/evil-lion.svg?branch=master)](https://travis-ci.org/edkolev/evil-lion)
[![MELPA](https://melpa.org/packages/evil-lion-badge.svg)](https://melpa.org/#/evil-lion)

![Emacs evil alignment operator](https://cloud.githubusercontent.com/assets/1532071/23845388/6edda00c-07d0-11e7-9ea1-ffa945f03980.png)

evil-lion
=========

This package provides `gl` and `gL` align operators: `gl MOTION CHAR` and right-align `gL MOTION CHAR`.

Use CHAR `/` to enter regular expression if a single character wouldn't suffice.

Use CHAR `RET` to align with align.el's default rules for the active major mode.

Port of [vim-lion](https://github.com/tommcdo/vim-lion)

![scar](https://cloud.githubusercontent.com/assets/1532071/23858247/7f33c4c6-0808-11e7-822c-e63c787c2f2b.png)

Installation
------------

#### with [use-package](https://github.com/jwiegley/use-package)
``` emacs-lisp
(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))
```

#### without [use-package](https://github.com/jwiegley/use-package)

`M-x package-install RET evil-lion RET`, then add in `init.el`:

`(evil-lion-mode)`

Usage
-----

Align with `gl MOTION CHAR` or right-align with `gL MOTION CHAR`.

If the align separator is `/` you will be prompted for a regular expression instead of a plain character.
If the align separator is `RET` alignment will be performed with align.el's rules specific for the major mode.

You can pass count `1` to align on the first occurrence of `CHAR`. To pass count, use: `COUNT gl MOTION CHAR`.

#### Example, left align `gl`:

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

#### Example, right align with `gL`:

After pressing `gLip,`
```
one, two, three,
fifteen, sixteen, seventeen
```

will become:

```
one,     two,     three,
fifteen, sixteen, seventeen
```

#### Example, align with major mode's alignment rules:

In perl-mode, after pressing `glib RET` (`RET` is return key, not individal keys):

``` perl
my %hash = (
   a => 1,
   bbb => 2,
   cccc => 3,

   a => 1,
   bbb => 2,
   cccccc => 3
);
```

will become:

``` perl
my $hash = (
   a    => 1,
   bbb  => 2,
   cccc => 3,

   a      => 1,
   bbb    => 2,
   cccccc => 3
););
```

#### Example, align on the first occurrence of CHAR:

After pressing `1glip"`
```
(red "red"
(teal-green "#6fb593")
(wheat "#b9c791")
(blue "blue")
(cyan "#54b6b6")
```

will become:

```
(red        "red"
(teal-green "#6fb593")
(wheat      "#b9c791")
(blue       "blue")
(cyan       "#54b6b6")
```

Customization
-------------

#### Disable squeezing of spaces

By default, evil-lion will remove unnecessary spaces if there are any. To disable this behaviour:

``` emacs-lisp
(setq evil-lion-squeeze-spaces nil) ;; default t
```

#### Change the default keys

``` emacs-lisp
;; use `g a` (mnemonic `align`)
;; these variables should be changed before (evil-lion-mode) is called
(setq evil-lion-left-align-key (kbd "g a"))
(setq evil-lion-right-align-key (kbd "g A"))
(evil-lion-mode)
```

Or with`use-package` and `bind-key`:

```
(use-package evil-lion
  :ensure t
  :bind (:map evil-normal-state-map
         ("g l " . evil-lion-left)
         ("g L " . evil-lion-right)
         :map evil-visual-state-map
         ("g l " . evil-lion-left)
         ("g L " . evil-lion-right)))
```

#### Bind in prog modes only

Bind `evil-lion-left` and `evil-lion-right` to your liking.
The `evil-lion-mode` is just a convenience mode and should not be enalbed with this setup.

``` emacs-lisp
(evil-define-key 'normal prog-mode-map
  (kbd "g l") 'evil-lion-left
  (kbd "g L") 'evil-lion-right)

(evil-define-key 'visual prog-mode-map
  (kbd "g l") 'evil-lion-left
  (kbd "g L") 'evil-lion-right)
```

