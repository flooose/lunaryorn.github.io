---
title: Colophon
layout: page
excerpt: |
  This blog is built with Jekyll and hosted on Github Pages.  The fonts are
  Vollkorn, Lato and Source Code Pro.  The source code is available on Github.
---

# Tooling

This blog is a static website built with pure [Jekyll][] on [Github Pages][].
[html-proofer][] checks the generated site for invalid HTML and other mistakes.

[Jekyll]: http://jekyllrb.com
[html-proofer]: https://github.com/gjtorikian/html-proofer

# Style and fonts

The serif font on this website is [Vollkorn][] by [Friedrich Althausen][fa].  It
comes with two stylistic sets.  The body text uses the standard, more
“old-style” set.  The headlines use the alternative stylistic set, which gives a
“plainer” look better suited for headlines, but still preserves the consistent
stylistic look of the whole body text.

The sans serif font is [Lato][].  I scarcely use it, but you'll find it in the
page title, and in hyperlinks.  The monospace font is [Adobe Source Code Pro][].
It is used for `inline code` and in source code snippets.

The colour scheme for syntax highlighting is [Solarized Light][].

The great book [Practical Typography][] by [Matthew Butterick][] provided much
help and guidelines for the style of this website.

# Source code, building and deployment

I keep the Markdown sources files, HTML templates and CSS stylesheets on
[Github][].  [Github Pages][] automatically builds and deploys the site after
every push.  [Travis CI][] checks and verifies the site independently.

[Solarized Light]: http://ethanschoonover.com/solarized
[Github]: https://github.com/lunaryorn/lunaryorn.github.io
[Matthew Butterick]: http://practicaltypography.com/end-credits.html#bio
[Practical Typography]: http://practicaltypography.com/
[Adobe Source Code Pro]: https://github.com/adobe-fonts/source-code-pro
[normalize]: http://necolas.github.io/normalize.css/
[Vollkorn]: http://vollkorn-typeface.com/
[fa]: http://friedrichalthausen.de/
[Lato]: http://www.latofonts.com/lato-free-fonts/
[Travis CI]: https://travis-ci.org/lunaryorn/lunaryorn.github.io
[Github Pages]: https://pages.github.com/
