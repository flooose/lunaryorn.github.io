---
title: "My Christmas present for fellow Emacs users: Flycheck 0.22"
published: 2014-12-23
tags: emacs,flycheck
---

With Christmas almost here, it's time for me to present my gift to you, my
fellow Emacs users: A brand new shiny release of Flycheck, now at version 0.22.
At the surface little has changed, but under the hood Flycheck made a big step
towards a 1.0 release, and provides even more features to write your own syntax
checkers.

The [release announcement][] and the [changelog][] have all the details, but in
this post I'd like to give you a short idea of what I think are the most
important features of this new release.

<!--more-->

# Use functions for syntax checking

The coolest feature in this release are “generic syntax checkers”.  I already
[wrote about them before](internal:posts/generic-syntax-checkers-in-flycheck.md),
so let's get to the point: They let you use arbitrary functions to check
buffers.  Sounds trivial, but let's use do cool new things.

Flycheck can now support languages that were hard or impossible to support
before ([OCaml][flycheck-ocaml] or [Clojure][squiggly-clojure]), have checkers
written in pure Emacs Lisp as in [flycheck-package][], an Emacs Lisp package
linter for Flycheck, or use special modes to improve syntax checking as in
[flycheck-irony][], which uses a background service based on libclang to check
C/C++ faster and better than the built-in C/C++ checkers.

# Test your syntax checkers

One of Flycheck's great strengths is the high quality of its code, which is
supported by a large test suite.  The code that drives this test suite is now
available for you as well:  The new `flycheck-ert` library is now part of all
Flycheck packages, and provides helpers and utilities to write unit tests for
Flycheck syntax checkers.

# Verify your setup

Another important feature of a totally different kind is the new
`flycheck-verify-setup` command: It's designed to help Flycheck users to find
errors in their setup.  It'll show you which syntax checkers are available for
the current buffer, and provide helpful hints if a syntax checker doesn't work.

# And more

There have been many more changes under the hood.  A notable one are error IDs:
Syntax checkers can now attach IDs to errors to identify them.  This feature is
currently not used a lot throughout Flycheck, but it's an important step towards
future extensions such as [automatically fixing errors][#530].

And of course, there's a new syntax checker, and many new options, improvements
and one or another bug fix.

I hope that you'll enjoy the new release, and I wish you a merry Christmas, and
a happy new year.

[release announcement]: http://flycheck.readthedocs.org/en/latest/guide/releases/flycheck-0.22.html
[changelog]: http://flycheck.readthedocs.org/en/latest/guide/releases/index.html#dec-23-2014
[flycheck-ocaml]: https://github.com/flycheck/flycheck-ocaml
[squiggly-clojure]: https://github.com/clojure-emacs/squiggly-clojure
[flycheck-package]: https://github.com/purcell/flycheck-package
[flycheck-irony]: https://github.com/Sarcasm/flycheck-irony/
[#530]: https://github.com/flycheck/flycheck/issues/530
