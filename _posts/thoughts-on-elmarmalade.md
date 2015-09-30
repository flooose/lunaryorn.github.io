---
title: Thoughts on elmarmalade
tags: emacs,marmalade,package,elnode,melpa
published: 2014-05-22
---

On [May 19th][post], Nic Ferrier switched the grand old [Marmalade][] to his
[elmarmalade][] rewrite of the old Javascript-based code.  He wrote elmarmalade
in Emacs Lisp, using his own Emacs Lisp web server [elnode][].  He [claims][1]
that this rewrite will be the solution to all the
[past problems of Marmalade](internal:posts/unbreaking-marmalade.md).

Currently, it seems to be far from that.  The site is almost non-functional.
Login and registration do not work yet, package uploads are not yet available,
and many links and anchors are broken.

That will likely get fixed sooner or later, but nonetheless, I am not convinced
that elmarmalade is really a good idea.

I doubt that [elnode][] is really a solid base to build a popular Emacs Lisp
archive on.  We frequently had issues with elnode when writing a simple
package.el server for use in the test cases of [Cask][], and generally perceived
it as unreliable and overly complicated without any real gain.  And we were
[not the only ones][2]…

Besides, Emacs itself is not a reliable and secure platform for non-interactive
programs, let alone web applications.  There is too much implicit state and too
much implicit behaviour in Emacs.

Nic Ferrier argues that Emacs Lisp is the right language for Marmalade, because
the Emacs Lisp community will have to maintain the archive, and would prefer to
do so in Emacs Lisp.  As a member of that community, I respectfully disagree.

Granted, Emacs Lisp is a nice language, but is not the only one.  There are
other good programming languages as well, and all of them have a lot more mature
web development stacks.  As nice as Emacs Lisp is, I do not think that having
Marmalade written in it is worth the effort of re-inventing an entire web
development stack again, and I doubt that it is the right tool for such an
endeavour.

Personally, I would have preferred if Marmalade had been re-written in a less
exotic (for web programming) language, with a standard, well-established and
rock-solid web framework, e.g. Django and Python.

By and large, I am not yet convinced that Marmalade is heading towards a better
future.  For my own packages, notably for Flycheck and its extensions, the
recommended archives are MELPA and MELPA Stable now.

[post]: http://lists.gnu.org/archive/html/emacs-devel/2014-05/msg00266.html
[elmarmalade]: https://github.com/nicferrier/elmarmalade
[Marmalade]: http://marmalade-repo.org/
[elnode]: https://github.com/nicferrier/elnode
[1]: https://github.com/nicferrier/marmalade/issues/73#issuecomment-35208242
[2]: https://github.com/eschulte/org-ehtml/commit/b9c21bb097561b8164f06406bb5dd866ddd3b5a5
[Cask]: https://github.com/cask/cask
