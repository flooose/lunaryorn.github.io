---
title: My journey to Spacemacs
---

In the last few months I was slowly gravitating towards Spacemacs, and about a
month ago I finally couldn’t resist its gravitational pull anymore.  I abandoned
my hand-made, beautiful, elaborate, 3k lines personal Emacs configuration in
favour of Spacemacs, and I’m now going to tell you why I did that, what the
switch was like, and how that whole thing feels.

<!--more-->

# Why?

What got me interested in Spacemacs initially was the awesome UI—yes, sorry 😊.
More precisely, the beautiful mode line.  I love beautiful things, and the
Spacemacs UI was just way more beautiful than what I managed to get to with my
configuration.  Even as I write this post in Spacemacs, after more than a month
of using it eight hours a day for my work I still marvel at its beauty 😍.

A beautiful UI makes me happy, but I must admit that it’s not getting my work
done.  If it had only been the UI it’d have been a short trip to Spacemacs for
me.  And even the beautiful UI didn’t overcome the nagging in the back of my
head which said “Hey, you’re using a starter kit, and you shouldn’t be!”.

## Made from one piece

But soon after installing and starting Spacemacs for the first time I realised
what Spacemacs really is: It’s not just a starter kit, **it’s an editor on its
own!** that just happens to be build upon Emacs.  And more than that: It’s a
real **product!**, and a great one, too.

Spacemacs doesn’t just tack a bunch of packages together with lots of glue, like
many starter kits do and like I did in my own configuration.  Spacemacs
*integrates* packages, and every time I use the incredibly consistent key
bindings, browse the perfectly arranged Helm buffers or see the beautiful
UI arrangements I notice that the difference is more than just words.  Spacemacs
appears incredibly consistent and beautiful; it really feels as if made from one
piece.

## Layers

It’s not just the surface that shines:  Spacemacs is well-designed from top
all down the way to it’s internals.  On the surface there are a great UI and a
consistent keybindings.  On the bottom there’s well-thought, powerful and
flexible configuration API which Spacemacs calls “layers”.

Layers successfully overcome a major problem in typical starter kits: The
opaqueness of their configuration.  There’s only so much that you change and so
far you can diverge from their opinions.  Sooner or later you hit the one
setting that you don’t like but can’t easily change either.  Starter kits
typically are a “take all or nothing” thing to a large degree.

Spacemacs however is a “take just what you like” story.  Its layers are entirely
transparent.  You can easily disable packages or override settings from other
layers, you can complement or even “steal” packages from other layers and even
replace layers entirely.  This enables you to pick only those parts that you
like.  You can omit the layers you don’t need, override the settings you don’t
like, disable the packages that you don’t want, “steal” the packages that you’d
like to configure differently, and complement those that you’d use in a
different way.

For instance, Spacemacs includes a simple setup for `exec-path-from-shell`,
a package which copies environment variables from your shell configuration.
This simple setup wasn’t nearly enough for me, so I just
[“stole” the package from Spacemacs][1]: I’m still using all of Spacemacs, but
I have my own and _only_ my own configuration for `exec-path-from-shell`.
Spacemacs doesn’t try to interfere; it just stays out of my way and let’s me
configure this package exactly the way I want, without affecting the rest of
Spacemacs.

Another example: Spacemacs adds yasnipppet which I heartly dislike 😠 But no harm
done, I just need to add it to `dotspacemacs-excluded-packages` and its gone
from my Emacs 😎

The layer system took me a while to understand, but in my opinion its one of the
best parts of Spacemacs and a really great way to structure an Emacs
configuration.  Actually I had completely misunderstood it initially, but the
Spacemacs maintainer took the time to explain the power and flexibility of the
system to me in a lengthy Twitter conversation.

[1]: https://github.com/lunaryorn/dotfiles/blob/8a310f16bbfc3fd8a122d4c661b36a23f1691dce/spacemacs/.spacemacs.d/layers/lunaryorn/packages.el#L42

## The community

That brings me to the next great thing about Spacemacs: The awesome community.
How often does a maintainer of a open source project take two hours of their
time to explain their software to you on Twitter?  Sylvain Benner is a great
maintainer, and an awesome person.  And it’s not just the maintainer
who’s beyond awesome, it’s the entire community.  They are patient, friendly,
helpful, supporting, competent, …

*It’s the kind of community I wish I had around Flycheck.*

## But what about VIM bindings?

I know that many people choose Spacemacs for its VIM bindings but for me that
wasn’t a big incentive.  With Spacemacs I learned to enjoy modal editing
again—I have been using VIM for quite some time in the past—and I like the
feeling of “programming” text instead of just editing it.  But it’s not that
important to me.  I didn’t miss modal editing when coming back to Emacs from VIM
and I have no problems with the Emacs way of editing either.

# How did I switch and what was it like?

I didn’t just jump head-first into cold water.  Re-training muscle memory for
new commands and new key sequences takes time.

Running parallel, switching back and forth, not sure whether staying with Spacemacs

# What I like less…

Mixed quality… some codes needs polishing

# Would I go back again?

No, I don’t think so.  I don’t know where my Spacemacs journey goes; I’m still
at beginning, slowly porting all my Emacs configuration into my new Spacemacs
layers.  But I don’t think that I’ll ever be able to use a bare-bones Emacs
again.

Emacs’s dead, long live Spacemacs!
