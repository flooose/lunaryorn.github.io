a month ago I finally couldn’t resist its gravitational pull anymore, and
abandoned my hand-crafted, elaborate, 3k lines personal Emacs configuration in
favour of Spacemacs.

In this post I’ll write about why I did that, what switching to a entirely
different Emacs configuration was like, and how that whole thing feels now.

<!--more-->

# You’re using a starter kit, why?!

What got me interested in Spacemacs initially was the beautiful user interface
😊.  Particularly, the beautiful mode line 😊.  I love beautiful things, and the
Spacemacs UI was just so much more beautiful than all that I had managed to get
to with my configuration.  Even as I write this post in Spacemacs, after almost
two months of using it eight hours a day for work, I still enjoy at its beauty
😍.  Nonetheless, I hesitated to commit myself to Spacemacs; I thought in the end
it’d just be like any other starter kit, only with different drawbacks and
limitations.

Spacemacs, however, is not just a starter kit.  It’s an editor on its own, with
a special focus on a beautiful user interface, a great user experience and a
deep integration of packages.  Spacemacs doesn’t feel like a bunch of packages
tacked together with lots of glue; it feels like a perfectly integrated product
made from one piece.

# Layers below

Under the surface, Spacemacs is powered by a well-designed and powerful
configuration API called “layers”.

----

Under the surface, a well-designed and powerful configuration API called
“layers” powers much of Spacemacs’ inner workings, and provides a solid based
for your own extensions to Spacemacs.

Layers elegantly solve a major problem in typical starter kits or configuration
bundles: Their configuration is *opaque*.  Often there is only so much that you
can change; there’s a limit in how far you can diverge from their opinions and
pre-built configurations.  At some point there’s the one setting or package that
you don’t like and that is impossible to change within the configuration bundle.

> To a large degree, configuration bundles are “take all or nothing”.

Spacemacs is not.  Its layers are entirely transparent.  They let you pick those
parts you need, and override or disable those that you don’t want.

The layer system took me a while to understand, but in my opinion its one of the
best parts of Spacemacs and a really great way to structure an Emacs
configuration.  If the GUI attracted me the layer system made me stay.

 completely misunderstood it initially, but the Spacemacs
maintainer took the time to explain the power and flexibility of the system to
me in a lengthy Twitter conversation.

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
