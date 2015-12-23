---
title: "Emacs Spotlight: Typographic Editing Modes"
tags: emacs-spotlight
category: emacs
layout: series-post
---

In this post I’ll introduce you to two modes that bring some typographic editing
features to Emacs: [Typo Mode][] and Tildify Mode.  These modes help you use
typographic quotes, punctuation and spaces.

<!--more-->

Typo Mode
=========

[Typo Mode][] is a 3rd party package that supports typographic quotes and
punctuation.  It comes in two flavours.  `typo-global-mode` is a global minor
mode which provides shortcuts for many special unicode characters under <kbd>C-c
8</kbd>, complementing the built-in <kbd>C-x 8</kbd> keymap.

`typo-mode` itself is more interesting.  This minor mode changes the behaviour
of some keys like <kbd>"</kbd>, <kbd>.</kbd> or <kbd>-</kbd> to cycle among
typographic variants of the corresponding character or to compose repeated
occurrences into a single typographic character.

For example, <kbd>"</kbd> inserts `“` (*left double quotation mark*).
Subsequent pressing of <kbd>"</kbd> cycles among `"` (*quotation mark*), `“`,
`”` (*right double quotation mark*), `‘` (*left single quotation mark*), and `’`
(*right single quotation mark*).  The initial character is context-aware: After
a left double quote the first character becomes `‘`, a single quotation mark,
that is.  Likewise, after an opening single quotation mark the first character
is `’`, i.e. the closing single quotation mark.

Typo Mode supports different languages with different quotation rules.  The
above shows the quotation rules of the English languages; Typo Mode also
supports Czech, German, French, Finnish, Russian and Italian.  Changing the
language with `M-x typo-change-language` changes both the initial character and
the subsequent cycling characters.  For instance, in German language cycling
starts at `„` (*low double comma quotation mark*) which is the opening quotation
character in German language.

In a similar way <kbd>-</kbd> cycles through different variants of hyphens and
dashes.  <kbd>.</kbd> continues to insert a normal dot, but inserting three dots
in a row will insert a unicode ellipsis `…`.

To enable Typo Mode add it to the hooks of the modes you’d like to use Typo Mode
in:

```cl
(add-hook 'text-mode-hook #'typo-mode)
```

If you use [Spacemacs][] enable the [Typography layer][layer].

[Spacemacs]: https://github.com/syl20bnr/spacemacs
[layer]: https://github.com/syl20bnr/spacemacs/tree/develop/layers/typography

Tildify Mode
============

Tildify Mode is a built-in mode in Emacs 25 and upwards that automatically
inserts [non-breaking spaces][nbsp] when appropriate.  A non-breaking space
is like a normal whitespace but inhibits word wrapping.  It is typically used
after single letters where a line breaking would be confusing and distracting.
For instance, it’s bad style to have a line break after “I”, because a single
letter at the end of a line is too easily overlooked.

A good word processor or a type setting system such as LaTeX normally handles
non-breaking spaces automatically, but the layout engines of browser or plain
text viewers are not that sophisticated and require explicit non-breaking spaces
to achieve a good text layout.  Tildify Mode helps you by inserting non-breaking
spaces at obvious places such as single letter words.

Like Typo Mode you can enable Tildify Mode by adding it to appropriate hooks,
e.g.

``` cl
(when (version<= "25" emacs-version)
  (add-hook 'text-mode-hook #'tildify-mode))
```

Tildify Mode is also part of the Typography layer of Spacemacs.

[Typo Mode]: https://github.com/jorgenschaefer/typoel
[nbsp]: https://en.wikipedia.org/wiki/Non-breaking_space
