---
title: On Uberspace now
tags: hosting,uberspace
published: 2014-06-11
---

A few days ago I moved this site from [Github Pages][] to [Uberspace][], a
unique little shared hosting provider from Germany with a large feature set and
an innovative payment concept: They let you pay as much (or as little) as you
want for their entire service, which includes generous storage limits, unlimited
mail accounts and addresses via IMAP, POP and SMTP, hosting of web applications
in various languages, full shell access, backups, and last but not least
outstanding technical support and a great admin team.

<!--more-->

The source of this site is still on Github, of course, even though Uberspace
also offers Git hosting[^1].  Github is simply the best, and I could keep most
of my setup this way.  Notably, I'm still using [Travis CI][] to build, test and
deploy the site, but instead of pushing to Github Pages the HTML goes to a
special repository on my Uberspace account, which has hooks in place to copy the
HTML files to where Apache can find them.

All this changes nothing for you, with two exceptions:

- The Github address (`http://lunaryorn.github.io`) doesn't work anymore.  This
  shouldn't affect you, though, since this site always used
  <http://www.lunaryorn.com> as the primary address, so you should never have
  encountered the Github Pages address anyway.
- I replaced Google Analytics with a self-hosted [Piwik][] instance, which
  aggressively anonymizes all collected data, and let's you opt-out from
  analytics with the standard [Do Not Track][] mechanism.

I also moved my mail from GMail to Uberspace, which lets me use addresses on a
custom domain.  My new mail address is listed in my [Github profile][].  The old
GMail address is forwarded to my new address, of course, but I'd encourage you
to use my new address right away.

In the process of migrating my mail, I also setup a separate mail address for
mails regarding [Flycheck][], which you can find on [Flycheck's profile][] page.

Apart from some little nuisances, and one or two pitfalls, including some very
serious fights with passphrase input in OpenSSH, the whole migration went quite
smoothly, and was indeed much easier than I had feared.

Ironically, I had also set up my own [Tiny Tiny RSS][] instance to replace
Feedly for news readings, just in time for today's [DDoS attack on Feedly][],
which consequently left me unimpressed :) The effort of migrating has already
doubly paid off.

[^1]: Even multi-user repositories, with [Gitolite][]

[Uberspace]: https://uberspace.de
[Github Pages]: https://pages.github.com/
[Gitolite]: http://gitolite.com/
[Travis CI]: https://travis-ci.org/lunaryorn/blog
[Piwik]: http://piwik.org/
[Do not Track]: http://en.wikipedia.org/wiki/Do_Not_Track
[Github profile]: https://github.com/lunaryorn
[Flycheck]: http://flycheck.readthedocs.org
[Flycheck's profile]: https://github.com/flycheck
[Tiny Tiny RSS]: http://tt-rss.org/redmine/projects/tt-rss/wiki
[DDoS attack on Feedly]: http://blog.feedly.com/2014/06/11/denial-of-service-attack/
