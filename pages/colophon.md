---
title: Colophon
layout: page
---

# Tooling

This blog is a static website built with a custom script based on the Haskell
library [Hakyll][].  It uses [Pandoc][] to render the Markdown source of posts
and pages to HTML.

# Style and fonts

The serif font on this website is [Vollkorn][] by [Friedrich Althausen][fa].  It
comes with two stylistic sets.  The body text uses the standard, more
“old-style” set.  The headlines use the alternative stylistic set, which gives a
“plainer” look better suited for headlines, but still preserves the consistent
stylistic look of the whole body text.

The sans serif font is [Lato][].  I scarcely use it, but you'll find it in the
page title, and in hyperlinks.  The monospace font is
[Adobe Source Code Pro][]—also my preferred Emacs font.  It is used for `inline
code` and in source code snippets.

The colour scheme for syntax highlighting is [Solarized Light][].

The great book [Practical Typography][] by [Matthew Butterick][] provided much
help and guidelines for the style of this website.

# Source code, building and deployment

I keep the entire source code of this site, including the Haskell code of the
build script, the HTML templates, custom CSS and the Markdown sources, on
[Github][].  [Travis CI][] automatically builds the site after every push and
deploys it to [Github Pages][].

[Pandoc]: https://github.com/jgm/pandoc "Pandoc"
[Hakyll]: https://github.com/jaspervdj/hakyll
[Bootstrap]: http://getbootstrap.com/
[Solarized Light]: http://ethanschoonover.com/solarized
[Github]: https://github.com/lunaryorn/blog
[Matthew Butterick]: http://practicaltypography.com/end-credits.html#bio
[Practical Typography]: http://practicaltypography.com/
[Adobe Source Code Pro]: https://github.com/adobe-fonts/source-code-pro
[normalize]: http://necolas.github.io/normalize.css/
[Vollkorn]: http://vollkorn-typeface.com/
[fa]: http://friedrichalthausen.de/
[Lato]: http://www.latofonts.com/lato-free-fonts/
[Travis CI]: https://travis-ci.org/lunaryorn/blog
[Github Pages]: https://pages.github.com/
