---
title: My Emacs
layout: page
excerpt: |
  I use Spacemacs for most of my daily work and maintain a couple of
  Emacs packages.
---

I use [Spacemacs][], a [GNU Emacs] distribution, for most of my daily work, with
some custom layers in my [dotfile][] repository.  I also maintain a couple of
Emacs packages:

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

[Spacemacs]: https://github.com/syl20bnr/spacemacs
[Flycheck]: http://www.flycheck.org
[Puppet Mode]: https://github.com/lunaryorn/puppet-mode
[ansible-doc]: https://github.com/lunaryorn/ansible-doc.el
[fancy-battery]: https://github.com/lunaryorn/fancy-battery.el
[pkg-info]: https://github.com/lunaryorn/pkg-info.el
[epl]: https://github.com/cask/epl
[GNU Emacs]: http://www.gnu.org/software/emacs/
[dotfile]: https://github.com/lunaryorn/dotfiles
