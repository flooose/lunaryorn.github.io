---
title: Autoloads in Emacs Lisp
---

On Reddit a user asked [how to use autoloads in Emacs Lisp][reddit].  This post
is essentially a summary and extension of my answers in that thread, but since
this topic is of interest to almost every Emacs Lisp developer, I thought I'd
share them to a wider audience.

<!--more-->

[reddit]: http://de.reddit.com/r/emacs/comments/29jp7c/autoload_questions_what_functions_should_i/

Autoloads
=========

Emacs Lisp offers an [autoloading mechanism][autoload] to load libraries on
demand.  Typically this is used to make interactive commands available to the
user without entirely loading the corresponding library upfront.

Autoloads are created with the [autoload](el-function:autoload) function.  An
autoload for the function `magit-status` in the library `magit.el` would look
like this:

```cl
(autoload 'magit-status "magit" "Open a Magit status buffer […]" t nil)
```

*Evaluating* this expression tells Emacs to automatically load the library
`magit.el` from `load-path`, when `magit-status` is called—either from Lisp, or
interactively with `M-x magit-status`—with no definition of this function yet
available.  You can add such autoloads to your `init.el` yourself in order to
autoload 3rd party libraries.  In the old days before package.el, this was very
common.

Note the emphasis on *evaluation*.  No autoloads are established by merely
writing this expression somewhere.  Emacs must evaluate it, which essentially
means that Emacs loads a file which contains this expression.

Now obviously it doesn't make any sense to put autoloads into the very same file
that also contains the definition of the autoloaded function.  Emacs would load
the rest of the file as well along with the autoload, which makes the definition
available right away and leads the purpose of autoloads ad absurdum.

[autoload]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload.html#Autoload

Autoload files
==============

Hence autoloads should be in a separate file, which ideally contains only
autoloads, and little else, so that it loads very fast.  Emacs calls these files
“autoload files”.

Autoload cookies
================

Manually maintaining autoload files to keep them in sync with the actual
definitions is tiresome and error-prone, so Emacs provides
[update-file-autoloads][ufa] and [update-directory-autoloads][uda] to automate
this process.

`update-file-autoloads` inspects the source files for special comments called
“autoload cookies”.  These cookies let you declare autoloads right at the
corresponding definition.  An autoload cookie for `magit-status` looks like
this:

```cl
;;;###autoload
(defun magit-status ()
  "Open a Magit status buffer […]"
  (interactive)
  ;; …
)
```

For each such cookie `update-file-autoloads` generates a corresponding
`autoload` like the one shown above, and writes it to the autoload file.
`update-directory-autoloads` performs this process for all files in a directory.

Note that these commands only *generate* autoload files.  You still need to
*explicitly load* the generated files to actually create the autoloads therein.

If an autoload cookie occurs on an expression with no special support for
autoloading, `update-file-autoloads` copies the expression verbatim.  This is
used to register libraries in specific Emacs extension points, like
`auto-modes-alist`.

[ufa]: el-function:update-file-autoloads
[uda]: el-function:update-directory-autoloads

Package autoloads
=================

Emacs' package manager `package.el` goes a step further still, and completely
automates autoloads by automatically generating autoload files during package
installation (using `update-directory-autoloads` internally).  This saves
package maintainers the tedious work of manually updating autoload files and
including them in their packages, and enables autoloads even for single-file
packages[^1].

These autoload files are automatically loaded by `package-initialize`, which is
typically called when Emacs starts.  Hence, all code in the autoload files,
whether real autoloads or arbitrary expressions is loaded at startup.

What to autoload?
=================

The general rule is to autoload *interactive “entry points”* of a package.
Examples of interactive entry points are:

- definitions of major and minor modes,
- interactive commands by which a user would start to use a specific package
  (e.g. `gnus`, `erc`, `magit-status`, etc.),
- and interactive commands which provide generic utilities, e.g. `occur`,
  `find`, `ace-jump-mode`, etc.

If your package just provides a library for use in Emacs Lisp code (e.g. like
dash.el or s.el), you do not need to add any autoloads at all.  Libraries are
typically `required`d, so autoloads are not necessary.

If your package should automatically register itself in specific Emacs extension
points, you should add autoloads for these as well, to make sure that they are
evaluated during package initialization.  A typical example is adding a mode to
`auto-mode-alist`:

```cl
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))
```

This puts `puppet-mode` into `auto-mode-alist` when Emacs starts, so that Puppet
Mode is automatically used for all files ending in `.pp`.

Likewise, colour themes use autoload cookies to add themselves to the color
theme search path:

```cl
;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))
```

Emacs Lisp API for autoloads
============================

Emacs Lisp has some functions to work with autoloads.  In addition to
[autoload](el-function:autoload) to create autoloads, there are
[autoloadp](el-function:autoloadp) and
[autoload-do-load](el-function:autoload-do-load).  The first lets you check
whether an object is an autoload object, and the latter loads the underlying
library of an autoload.

Note that both functions work on *autoload objects*, and *not* on symbols with
attached autoloads.  Hence, `(autoloadp 'foo)` checks whether the symbol `foo`
is autoloaded, which it obviously isn't: Symbols are not loaded at all.  They
are directly created by the reader, or explicitly with
[intern](el-function:intern).

To check whether `foo` refers to an autoloaded function you need to check the
*function definition* of `foo`:

```cl
(autoloadp (function-definition 'foo))
```

[^1]: Single file packages are standalone Emacs Lisp files with special file
      headers.
