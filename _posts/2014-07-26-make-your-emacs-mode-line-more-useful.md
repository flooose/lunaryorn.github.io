---
title: Make your Emacs Mode Line more useful
---

Customizing Emacs Mode Line was always some kind of black magic to me.  The
default mode line setup never worked quite well, but I never dared to customize
it.  Instead, I have been using packages such as Powerline or, more recently,
[Smart Mode Line][].  These are an improvement over the default mode line, but
never quite worked as I wished.

Recently, when I couldn't get Smart Mode Line to display battery info at the
place where I wanted it to be, I finally got rid of it, and decided to settle
down, read the manual, and work my way towards my own mode line setup.  Turns
out, that this is not at all difficult, and the result—even though far from
finished—is already quite pleasing:

<!--more-->

<figure>
<img src="/images/my-mode-line.png"
     alt="My custom mode line with Flycheck status, projectile project name, which function, etc."/>
<figcaption>My new mode line</figcaption>
</figure>

As usual, the Emacs Lisp reference helped greatly: The Mode Line customization
is thoroughly documented in the section [Mode Line Format][].

This post goes through my new mode line setup, and explains the details.

[Smart Mode Line]: https://github.com/Bruce-Connor/smart-mode-line
[Mode Line Format]:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html#Mode-Line-Format

Customizing the entire mode line
================================

The overall appearance of the mode line is set up in the
[mode-line-format](el-variable:mode-line-format) option.  After some digging and
fiddling, mine now looks as follows, which corresponds to the screenshot
above:

```cl
(setq-default mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                ;; Some specific information about the current buffer:
                lunaryorn-projectile-mode-line ; Project information
                (vc-mode lunaryorn-vc-mode-line) ; VC information
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                (multiple-cursors-mode mc/mode-line) ; Number of cursors
                ;; Misc information, notably battery state and function name
                " "
                mode-line-misc-info
                ;; And the modes, which I don't really care for anyway
                " " mode-line-modes mode-line-end-spaces))
```

Understanding `mode-line-format`
================================

`mode-line-format` is a list of “mode line constructs” which put specific
content into the mode line.  It is buffer-local by default, hence
`setq-default`.

A construct can be as simple as a string literal, like the whitespace strings
I'm using for spacing.  Strings can also contain special format strings, such as
the `%e` right at the beginning of the list.  These format strings are
substituted with various data, e.g. the position in the buffer, the current
column number, the name of the current buffer, or the currently visited file, or
in this case, a warning about low Lisp memory.  A list of all format strings is
available in the [%-Constructs][] section of the Emacs Lisp reference.

A construct can also be a symbol, in which case the variable value of that
symbol is used as mode line construct in place of the symbol.  This let's you
nicely break a complex mode line into manageable small pieces.  In fact, that's
how the default mode line is constructed: All the different `mode-line-*`
variables in the above example are built into Emacs, and describe various parts
of the mode line.  It also lets special modes such as EShell or Dired hook into
parts of the mode line, in case the standard format doesn't make much sense in a
specific mode.

The first part with standard information about the current buffer is just copied
from the default definition.  It shows the coding system, the modification
state, the buffer name and the position in the buffer, just like the default
mode line does.  That's just standard stuff, which should be in every mode line,
and the default order isn't too bad.

The individual elements are taken from various [Mode Line Variables][], which
you can customize individually, of course.  For instance, I'm using a more
compact design for the current position:

```cl
(setq-default mode-line-position
              '((-3 "%p") (size-indication-mode ("/" (-4 "%I")))
                " "
                (line-number-mode
                 ("%l" (column-number-mode ":%c")))))
```

But after that it gets really interesting.

[%-Constructs]: https://www.gnu.org/software/emacs/manual/html_node/elisp/_0025_002dConstructs.html#g_t_0025_002dConstructs
[Mode Line Variables]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Variables.html#Mode-Line-Variables

Adding custom mode line content
===============================

Normally, Emacs just displays the major mode and the list of minor modes after
the standard information, with `mode-line-modes`.  In my heavily customized
Emacs that's a really *long* list, which I'm not at all interested in.  I
usually know which modes are active: I enabled them in my `init.el` after all.

What I'm interested in is specific information about the current buffer from
various minor modes.  That's where more advanced mode line constructs join the
game.

The first thing I'd like to see is some information about the current project.
I'm using the great [Projectile][] extension, which identifies projects
automatically from certain files, and provides various commands and a useful API
to work with a project.

I define my own variable `lunaryorn-projectile-mode-line` which holds a
construct that puts the name of the current project into the mode line:

```cl
(defvar lunaryorn-projectile-mode-line
  '(:propertize
    (:eval (when (ignore-errors (projectile-project-root))
             (concat " " (projectile-project-name))))
    face font-lock-constant-face)
  "Mode line format for Projectile.")
(put 'lunaryorn-projectile-mode-line 'risky-local-variable t)
```

The mode line construct uses an `(:eval FORM)` construct to call out to the
Projectile API to get the name of the project the current buffer is part of.

The `(:eval FORM)` construct is a list that starts with the keyword `:eval`,
followed by a single Emacs Lisp form.  Whenever the mode line is updated, Emacs
evaluates the `FORM` and shows its results as a string in the mode line.  If the
result is `nil` the construct is ignored.

Around the `:eval` I wrapped a `(:propertize ELT PROPS…)` construct.  This
construct sets text properties on the string resulting from recursively
interpreting `ELT` as mode line construct.  I'm setting the most important
property of all: `face`.  That tells Emacs what face to use for the text.  I'm
using `font-lock-constant-face` which looks nicely in the colour theme I'm using
([Solarized Light][]).

Finally, I mark the variable as risky.  Otherwise, Emacs refuses to interpret
`:eval` and `:propertize` constructs for security reasons.

[Projectile]: https://github.com/bbatsov/projectile
[Solarized Light]: https://github.com/bbatsov/solarized-emacs

Showing specific minor modes
============================

After the project name I added some important minor modes, with `(SYMBOL WHEN
ELSE)` constructs.  These constructs describe conditional mode line content:

Emacs looks at the value of `SYMBOL`.  If it's non-nil, it recursively
interprets `WHEN` as mode line construct, otherwise it goes on with `ELSE`.  If
`ELSE` is omitted, the entire construct is ignored, if `SYMBOL` is nil.

This construct lets me show version control information, but only if `vc-mode`
is enabled, that is, if the current buffer is under version control.  Likewise,
I show the [Flycheck][] state, but only if Flycheck is enabled, and the number
of active cursors, but only if [Multiple Cursors][] is active.

The latter two just show the lighter of the corresponding minor mode, which is a
valid mode line construct by itself.  For VC Mode, I'm using my own mode line
construct that strips the backend name from the status.  I'm almost exclusively
using Git, and if I'm not doing so, I'm usually absolutely aware of that, if
only because Magit doesn't work in this case:

```cl
(defvar lunaryorn-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face font-lock-variable-name-face))
  "Mode line format for VC Mode.")
(put 'lunaryorn-vc-mode-line 'risky-local-variable t)
```

[Flycheck]: http://flycheck.readthedocs.org/en/latest/
[Multiple Cursors]: https://github.com/magnars/multiple-cursors.el

Miscellaneous information
=========================

The mode line sometimes also shows miscellaneous information about the current
buffer.  This information is stored in `mode-line-misc-info`.  In the default
mode line, this is almost the *last* element in `mode-line-format`, and pushed
so far to the right end of the mode line, that it's mostly not even visible,
because the list of minor modes takes too much space.

I'm putting it before the list of modes, because it's much more relevant to me
than the list of modes.  Notably, it shows the name of the function the point is
in with `which-func-mode`, and the state of my laptop's battery with
`display-battery-mode`.  You can also show the current date and time with
`display-time-mode`, but I'm not doing so, because I typically carry a watch, or
look on my phone or a wall clock if I need to know the current time.

Conclusion
==========

My mode line setup is still far from perfect, notably I'd like to find some
better presentation for the list of minor modes (any help on that is
appreciated), probably in some kind of popup menu.  But it's already far better
than anything I had before, and against demonstrated to me the unrivalled
customizability of Emacs.
