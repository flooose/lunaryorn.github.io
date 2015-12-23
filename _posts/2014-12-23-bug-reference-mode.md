---
title: "Emacs Spotlight: Bug Reference Mode"
category: emacs
tags: emacs-spotlight
layout: series-post
---

There's not a day in which I don't find a new gem for Emacs.  Today it's the
built-in `bug-reference-mode`.  This fancy little thing turns issue references
in text into clickable buttons that browse the corresponding issue in a bug
tracker.  I discovered it via the [bug-reference-github][] package, which
automatically configures the mode for files in Github repositories.

<!--more-->

I don't use that package, though, because I don't feel that it's necessary:
[Directory Variables][] are easy enough.  Just use `M-x add-dir-local-variable`
to set `bug-reference-url-format` to the URL of your bug tracker.

For instance, I have the following `.dir-locals.el` for [Flycheck][]:

```cl
((nil
  (bug-reference-url-format . "https://github.com/flycheck/flycheck/issues/%s"))
 (rst-mode
  (bug-reference-bug-regexp . "\\[GH-\\(?2:[[:digit:]]+\\)]")))
```

This sets `bug-reference-url-format` for all files in Flycheck to the URL of the
Github issue tracker.  The URL string is passed through `format` with the issue
ID as argument, so be sure to have an `%s` escape in the URL at the proper
place.

In source code, I tend to use pretty standard references such as `bug #111`,
which are caught by the standard `bug-reference-bug-regexp`. The
[changelog of Flycheck][changelog] however uses a more structured format for bug
references, to make it easier to parse issue references out of the changelog.  A
reference to the bug 111 will look like `[GH-111]`.

Hence, I set `bug-reference-bug-regexp` for `rst-mode` (the changelog is written
in in ReST) to match this format.  Note the peculiar `(?2:…)` grouping: For
whatever mysterious reason, `bug-reference-mode` expects that the *second*(!)
group in the regexp matches the issue ID.  Hence my regexp uses an explicitly
numbered group for the ID—one of the lesser known features of Emacs.

Now all I am left to do is turn on `bug-reference-mode` in my `init.el`:

```cl
(add-hook 'text-mode-hook #'bug-reference-mode)
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)
```

`bug-reference-prog-mode` is like `bug-reference-mode`, but only for comments
and strings, which is generally what you want in source code.  Just like
`flyspell-prog-mode` basically.

Next time I visit the changelog, all bug references are emphasized and turned
into buttons.  I can either use the mouse to click on them, or press `C-c Ret`
while the point is on a bug reference, to open the corresponding bug on the
Github webpage.

[bug-reference-github]: https://github.com/arnested/bug-reference-github
[Directory Variables]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html#Directory-Variables
[Flycheck]: https://github.com/flycheck/flycheck
[changelog]: https://github.com/flycheck/flycheck/blob/master/CHANGES.rst
