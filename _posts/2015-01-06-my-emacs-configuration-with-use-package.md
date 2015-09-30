---
title: My Emacs Configuration with use-package
---

In the past I used to keep my Emacs configuration completely in a single
`init.el` file.  For a long time this worked quite well, but of late my
configuration became increasingly messy: Package configuration was mixed with
utility functions, key bindings, and even larger code for entirely new features.
Needless to say that my init file was in dire need of a very thorough cleanup.

I had heard a lot of good things about John Wiegley's [use-package][] macro, and
in the days after Christmas I decided to sit down and try to refactor my Emacs
configuration with `use-package`.  The result was very pleasant, and much better
than I had dared to hope.

<!--more-->

[use-package]: https://github.com/jwiegley/use-package/

The basics
==========

The idea of `use-package` is to wrap all initialisation and configuration of a
package in a top-level form.  A typical use in my configuration looks like this:

```cl
(use-package whitespace
  :bind (("C-c T w" . whitespace-mode))
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config (setq whitespace-line-column nil)
  :diminish whitespace-mode)
```

This form binds Whitespace Mode to <kbd>C-c T w</kbd> globally, enables it
automatically for certain modes, and configures it.  `:bind` and `:init` are
evaluated immediately, whereas `:config` is deferred until after the package is
loaded, similar to `with-eval-after-load`[^2].  `:diminish` is just a shortcut
for the [diminish][] utility which removes minor modes from the mode line.

Now compare this to the same code *without* `use-package`, as it would appear in
my init file before:

```cl
(global-set-key (kbd "C-c T w") #'whitespace-mode)

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'whitespace-mode))

(with-eval-after-load 'whitespace
  (setq whitespace-line-column nil)
  (diminish 'whitespace-mode))
```

Clearly the `use-package` variant is more concise and organised, and much easier
to understand.  It keeps everything related to a package in a single top-level
form, which puts all of the scattered package initialisation and configuration
code together.  This alone made my init file much easier to understand, but the
real power of `use-package` does not end here—in fact, I have not show you any
of the really cool stuff yet!

[diminish]: https://github.com/emacsmirror/diminish

Automatic package installation
==============================

These days most of the cool Emacs Lisp isn't built-in like `whitespace-mode` but
comes from [MELPA][].  I have almost 100 3rd party packages in my Emacs.  I'd be
a huge pain to track and install these manually whenever I remove the package
directory or move to a new machine, but with `use-package` I don't have to[^1].
`use-package` can automatically install missing packages:

```cl
(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)))
```

This feature is so convenient that I mostly stopped to install new packages via
<kbd>M-x list-packages</kbd>.  Now I add a `use-package` form for any new
package to my init file right away, with some basic initialisation and
configuration—usually from the Github README of the package—and an `:ensure`
keyword, and type <kbd>C-M-x</kbd> to evaluate the form to install and setup the
package in one go.

I still need to bootstrap `use-package` explicitly at the beginning of my init
file, though.  This is not that pretty, but the obvious chicken-egg problem
can't be avoided otherwise:

```cl
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```

[MELPA]: http://melpa.org

“Local” packages
================

While I try to use packages as much as possible, and also release most of my
custom code as packages to [MELPA][], I still have some code in my configuration
that is too small or too specific to my own workflow and my personal preferences
to be released independently.

`use-package` makes it easy to maintain this code.  I can keep it in separate
libraries, pretending that they are proper packages installed with the package
manager, and use `use-package` as usual to load my custom code.  For example, I
have a `lunaryorn-simple` library which contains many small helper functions for
editing.  It sits in the `lisp/` subdirectory of my Emacs directory and is never
installed with the package manager, but `use-package` lets me configure as if it
were:

```cl
(use-package lunaryorn-simple
  :load-path "lisp/"
  :bind (([remap kill-whole-line]        . lunaryorn-smart-kill-whole-line)
         ([remap move-beginning-of-line] . lunaryorn-back-to-indentation-or-beginning-of-line)
         …
         ("C-c u d"                      . lunaryorn-insert-current-date)))
```

The only special thing is `:load-path`, which adds the containing directory to
Emacs' `load-path` so that it can find my personal library.  But I don't need to
care for autoloads and lazy loading: `use-package` automatically adds autoloads
for all commands bound to keys in `:bind`.  My library is loaded lazily when I
invoke any of these commands, just like a regular package installed via the
package manager.

With this feature I can keep my init file (almost) free of any code.  It only
contains package configuration now.  My custom code is neatly tucked away in
separate libraries that look just like regular Emacs packages.  This does not
only make my configuration easier to understand, it has also fundamentally
changed my package development workflow.

Most of my packages are born out of small customisation and personal functions
that grow as I extend them, until they are large and stable enough to be
released as separate packages.  Previously, making a package out of these was
painful: I had to manually extract all the required code form my init file and
fix various compiler warnings and errors, naturally making many mistakes on the
way.

Now I start with a separate library right away, which is a proper package on its
own.  All code goes through [Flycheck][] to make sure that there are no errors
or warnings.  Once the package is suitable for an independent release, there's
no special work left: It's all already there, and all that I still need to do is
to move the file to a dedicated repository, add a README, and push it to MELPA.
I think you can expect quite some new packages from me over the next time!

[Flycheck]: http://flycheck.org

Idle initialisation
===================

`use-package` also helps me to keep my Emacs startup fast with “idle
initialisation”, which initialises packages after Emacs was started and has been
idle for some time.  I use this feature mainly for global modes that are slow to
load and enable.

[Company][] for instance is a powerful completion package, but it also large and
takes time to load and enable.  On the other hand, completion is not so
important for me that I need it immediately, so I delay its initialisation:

```cl
(use-package company
  :ensure t
  :defer t
  :idle (global-company-mode))
```

With this configuration `global-company-mode` is delayed until Emacs has been
idle.  As a result, Emacs starts faster: Packages of less importance do not
contribute to startup time anymore.  They are initialised later, when Emacs
doesn't have to do anything else anyway.

[Company]: http://company-mode.github.io/

Final words
===========

`use-package` is really a great tool to manage and maintain your init file,
which helps to keep even large configurations concise and clean and avoids the
dreaded Emacs bankruptcy.  Take a look at the [Github Page][use-package] and
read its README, which shows even more cool features than this post.

I'd like to thank John Wiegley for this great package, and for all his other
work on Emacs!

[^1]: Actually, I never managed my packages manually.  Before `use-package` I
      kept a list of packages at the beginning of my init file, together with
      some custom code to install all missing packages automatically.  This did
      not work too well, though: Frequently I forgot to update the list when I
      installed a new package and ended up with load-time errors.

[^2]: Strictly speaking, `:config` is only deferred in some cases, i.e. with
      `:defer t`, or when using `:bind`, `:commands` or a similar keyword—the
      [documentation][use-package] has the details.  But in almost all cases you
      want `:config` to be deferred, so for simplicity let's assume that
      `:config` is always deferred.
