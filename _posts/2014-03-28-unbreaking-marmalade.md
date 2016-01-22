---
title: Unbreaking Marmalade
category: emacs
---

**Update** *(June 16, 2014)*: Nic Ferrier's
[reimplementation of Maramalade](https://github.com/nicferrier/elmarmalade) went
live some time ago, and I'd like to use that occasion to retract my angry and
stupid words below.

While I do not think that the approach is right, and am not convinced that
Marmalade heads towards a better future, I think that in and by itself Nic
Ferrier did a great work, and delivered a great piece of software that is
probably pushing the boundaries of what can be done with Emacs farther than
anything before it.  For the sake of the Emacs community, I truly hope that the
new Marmalade becomes a real success.

I'd like to apologize for what I have said below, and send my best wishes to
Nic.  I am truly grateful for what he did.

----

**Update** *(April 28, 2014)*: [Binaries for Linux and OS X][releases] are
available now.  The installation instructions below were updated accordingly.

----

**Update** *(April 1, 2014)*: It's on [Hackage][] now.  The installation
instructions below were updated accordingly.

----

<figure>
<img src="{{site.baseurl}}/images/marmalade-crash.png"
     alt="A stacktrace from Marmalade"/>
<figcaption>The dreaded Marmalade Screen of Death</figcaption>
</figure>

Looks familiar?  Then you're probably a fellow Emacs developer trying to upload
a package to the [Marmalade][] ELPA archive.

We've been getting this fancy error for some time now, and there's already a
nice [issue][] for it, but little progress.  Just like, er, well… in the
entire Marmalade itself.  Sure, there is Nic Ferrier's brand-new Emacs Lisp
Marmalade rewrite super-thing, but… ok, even Duke Nukem made it to a release
eventually, so we have some hope left, haven't we?

And while we're excitedly waiting for Marmalade being reborn like a phoenix from
the ashes, I'd like to share [marmalade-upload][] with you.  It's a little
Haskell tool to upload packages to Marmalade via its API, which isn't broken yet
fortunately (don't hold your breath, folks).

To get started, download the appropriate binary for your system from the
[Release page][releases], unzip the archive and move the contained
`marmalade-upload` executable into a directory in your `$PATH`,
e.g. `/usr/local/bin/`.

If there are no binaries for your system, install [Haskell Platform][] and use
the following command to build and install `marmalade-upload` from source:

```
$ cabal install marmalade-upload
```

It will take a while to fetch and build all the dependencies of
`marmalade-upload`.  The resulting binary ends up in `~/.cabal/bin`, so add this
directory to your `$PATH`.

Once installed, you can upload your new packages with `marmalade-upload USERNAME
PACKAGE-FILE`, e.g.:

```
$ marmalade-upload lunaryorn dist/flycheck-0.18.tar
```

The tool will ask for your Marmalade password, and upload the package.  For
extra safety, it checks the mimetype of the file first, and throws an error if
it's invalid.  If you're on OS X or KDE, the tool will put your Marmalade login
token (not your Marmalade password!) into the keychain, so you won't be prompted
for your password next time.

And, yes, sorry for the inconvenient language, but it's a boring tool for a
stupid bug, so I at least try to learn a bit from it by refreshing what little
of my Haskell skills is still there.

In a related development, [MELPA][] recently [announced][] that they are
building [stable packages][] now from DVCS (currently Git only) tags, which is a
*big* step closer to Marmalade's final passing.  So long, dear Marmalade, we had
hard time with you, and we're glad that's over soon.

[Hackage]: http://hackage.haskell.org/package/marmalade-upload
[releases]: https://github.com/lunaryorn/marmalade-upload/releases
[Marmalade]: http://marmalade-repo.org/
[issue]: https://github.com/nicferrier/marmalade/issues/73
[marmalade-upload]: https://github.com/lunaryorn/marmalade-upload
[Haskell Platform]: http://www.haskell.org/platform/
[MELPA]: http://melpa.milkbox.net/
[announced]: http://www.reddit.com/r/emacs/comments/216jhc/stable_packages_from_melpa/
[stable packages]: https://github.com/milkypostman/melpa#stable-packages
