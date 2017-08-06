
(require 'ert)
(require 'evil)
(require 'evil-lion)
(require 'evil-test-helpers)

(evil-lion-mode)

(ert-deftest evil-lion-test ()
  :tags '(evil-lion)
  (dolist (squeeze '(t nil))
    ;; run the following tests with both squeeze-spaces On and Off,
    ;; the result should be the same
    (setq evil-lion-squeeze-spaces squeeze)
    (ert-info ("Simple")
      (evil-test-buffer
       "
[o]ne = 1
three = 3
fifteen = 15
"
      ("glap=")
      "
one     = 1
three   = 3
fifteen = 15
"))
  (ert-info ("Multiple separators")
    (evil-test-buffer
      "
[a]aaaaa|bbb|ccc
aa|bbbbbbb|cc
"
      ("glap|")
      "
aaaaaa|bbb    |ccc
aa    |bbbbbbb|cc
"))
(ert-info ("Right-align multiple separators")
  (evil-test-buffer
   "
[a], b, c
aa, bb, cc
aaa, bbb, ccc
"
      ("gLap,")
      "
a,   b,   c
aa,  bb,  cc
aaa, bbb, ccc
"))
(ert-info ("Right-align multiple separators on first occurance")
  (evil-test-buffer
   "
[a], b, c
aa, bb, cc
aaa, bbb, ccc
"
      ("1gLap,")
      "
a,   b, c
aa,  bb, cc
aaa, bbb, ccc
"))

(ert-info ("Align by dot")
  (evil-test-buffer
   "
[a]. b. c
aa. bb. cc
aaa. bbb. ccc
"
      ("gLap.")
      "
a.   b.   c
aa.  bb.  cc
aaa. bbb. ccc
"))
(ert-info ("Align by dot")
  (evil-test-buffer
   "
[a]X bX c
aaX bbX cc
aaaX bbbX ccc
"
      ("gLap/X")
      "
aX   bX   c
aaX  bbX  cc
aaaX bbbX ccc
"))
(ert-info ("Align visual selection")
  (evil-test-buffer
   "
<[a], b, c
aa, bb, cc>
aaa, bbb, ccc
"
   ("gL,")
   "
a,  b,  c
aa, bb, cc
aaa, bbb, ccc
"))
(ert-info ("Align only specified text object")
  (evil-test-buffer
   "
a, b, c
aa, bb, cc
aaa, bbb, ccc

[a], b, c
aa, bb, cc
aaa, bbb, ccc

a, b, c
aa, bb, cc
aaa, bbb, ccc
"
   ("gLip,")
   "
a, b, c
aa, bb, cc
aaa, bbb, ccc

a,   b,   c
aa,  bb,  cc
aaa, bbb, ccc

a, b, c
aa, bb, cc
aaa, bbb, ccc
"))
(ert-info ("Plain (align) in perl-mode")
  (evil-test-buffer
   :visual-end "}" ;; default is ">"
   "
my %hash = (
   [a] => 1,
   bbb => 2,
   cccc => 3,

   a => 1,
   bbb => 2,
   cccccc => 3
);"
   (perl-mode)
   ("glib" (kbd "C-m"))
   "
my %hash = (
   a    => 1,
   bbb  => 2,
   cccc => 3,

   a      => 1,
   bbb    => 2,
   cccccc => 3
);")))


(ert-info ("Align with and without squeeze spaces")
  (evil-test-buffer ;; align and squeeze the spaces, but ignore spaces not followed by the regex
   "[a]    = 1,  one
b   = 2 , two
c  = 3 , three
"
   (setq evil-lion-squeeze-spaces t)
   ("glip=")
   "a = 1,  one
b = 2 , two
c = 3 , three
"
   )
  (evil-test-buffer ;; same test as above, but without sqeezing the spaces
   "[a]    = 1,  one
b   = 2 , two
c  = 3 , three
"
   (setq evil-lion-squeeze-spaces nil)
   ("glip=")
   "[a]    = 1,  one
b    = 2 , two
c    = 3 , three
"
   ))
(ert-info ("Rigth align with squeeze spaces")
  (setq evil-lion-squeeze-spaces t)
  (evil-test-buffer
   "[a] ,                b
aa ,           bb
aaa ,   bbb
"
   ("gLip,")
   "a ,   b
aa ,  bb
aaa , bbb
"))
(ert-info ("Left align with sqeezing spaces")
  (setq evil-lion-squeeze-spaces t)
  (evil-test-buffer
   "a    =   1
b   =   2
c  = 3
"
   ("glip=")
   "a =   1
b =   2
c = 3
"))
(ert-info ("Right align with squeeze spaces and COUNT 1")
  (evil-test-buffer
   "1,   1,    1
22,   2,            2
"
   ("1gLip,") ;; test with COUNT 1
   "1,  1,    1
22, 2,            2
"))
(ert-info ("Right aligning already aligned with squeeze spaces and COUNT 1")
  (evil-test-buffer
   "1,  1,    1
22, 2,            2
"
   ("1gLip,")
   "1,  1,    1
22, 2,            2
"))

(ert-info ("Simple, spaces should be squeezed")
  (evil-test-buffer
   "
[o]ne  = 1
three  = 3
fifteen  = 15
"
      ("glap=")
      "
one     = 1
three   = 3
fifteen = 15
"))

(ert-info ("Simple, spaces should be squeezed, but not added")
  (evil-test-buffer
   "
[o]ne  = 1
three  = 3
fifteen= 15
"
      ("glap=")
      "
one    = 1
three  = 3
fifteen= 15
")))
