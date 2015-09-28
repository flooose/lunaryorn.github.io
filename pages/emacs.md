---
title: My Emacs
layout: page
---

I use [GNU Emacs][] for most of my daily work.  You can find my configuration on
[Github](https://github.com/lunaryorn/.emacs.d/), as a single init file based on
[use-package][] with some additional libraries.  My configuration requires Emacs
trunk, and will not work on any stable Emacs release.

Most of it is just customisation for various packages, but there are some gems
inside.  Notably, I've spend some time to create a nice mode line, and there are
quite elaborate configurations for AUCTeX/RefTeX, Haskell Modex, and Emacs Lisp.
It's also quite tuned for OS X.

I also maintain a couple of Emacs packages:

- [Flycheck][] is an on-the-fly syntax checking extension, intended as
  replacement for Flymake, with better performance, more supported languages and
  cooler features.
- [Puppet Mode][] is a major mode for Puppet 3 manifests.  Puppet is a
  configuration and provisioning tool.
- [ansible-doc][] lets you read the documentation of Ansible modules in Emacs.
- [fancy-battery][] shows the battery status in the mode line.  It's like the
  built-in `display-battery-mode`, but fancier and more customisable.
- [pkg-info][] is a library to obtain information about installed
  packages.  Notably it lets you query the versions of installed packages.
- [EPL][] is a library to work with the Emacs package manager.  It lets you
  query the package database, install or remove packages, and perform upgrades.

[Flycheck]: http://flycheck.readthedocs.org
[Puppet Mode]: https://github.com/lunaryorn/puppet-mode
[ansible-doc]: https://github.com/lunaryorn/ansible-doc.el
[fancy-battery]: https://github.com/lunaryorn/fancy-battery.el
[pkg-info]: https://github.com/lunaryorn/pkg-info.el
[epl]: https://github.com/cask/epl
[GNU Emacs]: http://www.gnu.org/software/emacs/
[use-package]: https://github.com/jwiegley/use-package/
