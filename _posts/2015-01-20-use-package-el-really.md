---
title: Why package.el?
---

Today, an Emacs user on Reddit asked
[how to organise their Emacs extensions][1], specifically whether to use
package.el or a “home-made” solution.  This post answers that question.  It's
essentially a copy of a [Reddit comment of mine][2] from about a year ago.

<!--more-->

----

package.el goes beyond just fetching files, and

- installs dependencies of a package,
- adds packages to the `load-path`,
- byte-compiles the package files,
- generates and loads autoload definition (improves startup time, since package
  files are loaded on demand),
- and registers manuals for the built-in Info viewer (lets your read manuals of
  packages right in Emacs).

The differences by example: Suppose you want to use [Flycheck][] (shameless
plug, sorry :) ), with all of the above features. You'd need to

- manually install about six packages which are direct or indirect dependencies
  of Flycheck,
- clone or update the Flycheck repository,
- add the Flycheck directory to `load-path`,
- run `make compile`,
- run `M-x update-directory-autoloads` on the Flycheck directory,
- load `flycheck-autoloads.el` in your `init.el`,
- run `make info` (make sure that you have Texinfo, Python, Sphinx and a bunch
  of other packages installed),
- and add the `doc/` sub-directory of the Flycheck directory to
  `Info-directory-list`.

And most of these **every time** you update Flycheck.  With package.el, however,
you'd just add the MELPA archive and run `M-x package-install RET flycheck`.
*See the difference? Does your manual workflow really work well?*

Besides, as a package author, I consider package.el as the only official
distribution channel for my extensions, and I refuse to provide support for
other installation methods, so if you screw up on your manual installation,
you're on your own.

[1]: http://www.reddit.com/r/emacs/comments/2t1886/organising_extensions_git_submodule_vs/
[2]: http://www.reddit.com/r/emacs/comments/1z9awm/to_use_packageel_or_not/
[Flycheck]: http://flycheck.readthedocs.org/en/latest/
