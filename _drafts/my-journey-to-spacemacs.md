---
title: My journey to Spacemacs
---

Over the last six months I was slowly gravitating towards Spacemacs.  About a a
month ago I finally couldn’t resist its gravitational pull anymore, and
abandoned my hand-crafted, elaborate, 3k lines personal Emacs configuration in
favour of Spacemacs.

In this post I’ll write about why I chose Spacemacs, what switching to a
entirely different Emacs configuration was like, and how that whole thing feels
now.

<!--more-->

## You’re using a starter kit, why?! ##

> Spacemacs is beautiful.

What got me attracted to Spacemacs initially was the beautiful user interface,
particularly the beautiful sleek mode line.  I love beautiful things, and the
Spacemacs UI was just so much more beautiful than all that I had managed to
achieve in my own configuration.  Even as I write this post in Spacemacs, after
almost six months of using it eight hours a day for work, I still enjoy at its
beauty.

> Spacemacs is not a starter kit—it’s an editor on its own!

What made me stay with Spacemacs finally was not its beauty, though; it was the
revelation that Spacemacs—unlike Prelude, Graphene and similar projects—is *not*
a “starter kit”.  On the contrary: Spacemacs is actually an editor on its own,
with a special focus on a beautiful user interface, great user experience and
deep integration of packages.  It feels like a perfectly integrated product made
from one single piece.

## Layers below ##

As bright as Spacemacs shines on the surface, as solid are it’s underpinnings
and foundations, notably it’s excellent, well-designed and powerful
configuration system called “layers”.  A layer groups related packages and
configuration into a single unit that provides a specific feature.  Each layer
can be activated as a whole.  For instance the `git` layer provides support for
the Git version control system and enables the popular [Magit][] extension and a
couple of other Git-related packages.

> Layers are transparent.  They _never_ get in your way.

But—and this is what’s so particularly awesome about layers—layers are not
opaque.  A layer is not a “take all or nothing” thing.  Each individual package
within a layer can be disabled in isolation without breaking the layer at large.
It’s even possible to “steal” a package in another layer, thus using a layer,
but with an entirely different configuration for a specific package.

> Layers enable horizontal *and* vertical customisation.

Unlike conventional starter kits, Spacemacs’ layer system succeeds at enabling
customisation in both dimensions: Horizontally by adding new layers to your
customisation, but also vertically by changing individual parts of a layer—which
is what all other starter kits fail to provide.

> Layers give structure and guidance to your personal configuration.

Spacemacs includes [many built-in layers][built-in-layers], but also lets users
define their own layers and thus gives you structure and guidance to organise
your configuration without risking the dreaded “Emacs bankruptcy”.  Spacemacs
configuration works best if you create layers early on for every additional
configuration, and leave Spacemacs’ init file (`~/.spacemacs` or
`~/.spacemacs.d/init.el`) only for Spacemacs’ own settings (as in the template)
and the list of enabled layers and excluded packages.

The **external structure** of layers helps you to group related configuration
into single “units” or larger features.  Spacemacs encourages you to follow its
own example and not create a single large layer for your entire configuration,
but rather split your configuration into small isolated layers, where each layer
works as a single unit that provides one consistent larger feature.

The **internal structure** of a layer in turn helps you to modularise your
configuration around the concept of packages, where each package provides one
individual feature of a layer (e.g. completion, compilation, syntax
highlighting, etc.).  Layer packages are mostly identical with ELPA
packages—Spacemacs will try to install packages from ELPA by default—but they
don’t have to be: Spacemacs also knows “local” packages, i.e. libraries that are
contained within a layer, which allows to keep a layer itself free from large
amounts of custom extension code.  Custom code goes into local packages, which
are then enabled in the package configuration of a layer.

[Magit]: http://magit.vc
[evil-magit]: https://github.com/justbur/evil-magit
[built-in-layers]: http://spacemacs.org/layers/LAYERS.html

## Dark corners ##

TODO

## But what about VIM bindings? ##

Many people choose

I know that many people choose Spacemacs for its VIM bindings but for me that
wasn’t a big incentive.  With Spacemacs I learned to enjoy modal editing
again—I have been using VIM for quite some time in the past—and I like the
feeling of “programming” text instead of just editing it.  But it’s not that
important to me.  I didn’t miss modal editing when coming back to Emacs from VIM
and I have no problems with the Emacs way of editing either.

## How did I switch and what was it like? ##

I didn’t just jump head-first into cold water.  Re-training muscle memory for
new commands and new key sequences takes time.

Running parallel, switching back and forth, not sure whether staying with Spacemacs

