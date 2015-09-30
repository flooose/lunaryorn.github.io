---
title: Quoting emacs-devel
---

I just came across this tweet by
[Artur Malabarba](https://twitter.com/AMalabarba):

<blockquote class="twitter-tweet" lang="de"><p lang="en" dir="ltr"> I just made
a rough count. The curly quotes issue has generated over 100k words on the <a
href="https://twitter.com/hashtag/emacs?src=hash">#emacs</a> dev list.
That&#39;s 3 times my doctoral thesis.</p>&mdash; Artur Malabarba (@AMalabarba)
<a href="https://twitter.com/AMalabarba/status/641598687467163648">9. September
2015</a></blockquote> <script async src="//platform.twitter.com/widgets.js"
charset="utf-8"></script>

The issue of using curly quotes (`‘foo’`) in docstrings and other places where
Emacs has of old used ASCII quotes (`` `foo'``), that is.

I’m furious that they apparently did not seriously consider the use of Taiwanese
quotation marks (`『foo』`)[^1].  In the end, though, the depth and complexity
of this matter might just warrant new Unicode codepoints.  I propose
`HALF-NOT-SO-CURLY OPENING EMACS QUOTE` and `ALMOST BUT NOT QUITE ENTIRELY
UNCURLY CLOSING EMACS QUOTE`.

I hope that they escalate the matter timely to the Unicode committee, before
they add the fourth PhD thesis to the discussion[^4].

[^1]: The
    [Wikipedia page on quotation marks](https://en.wikipedia.org/wiki/Quotation_mark#Summary_table_for_all_languages)
    makes me sad.  Quotation marks are disappointingly standardised.  Every
    language uses more or the less the same.  C’mon, is that really all you can
    offer?

[^4]: In a related development, I propose “units of PhD thesis”
    as a new international SI unit for the amount of words wasted in entirely
    irrelevant nit-picking.
