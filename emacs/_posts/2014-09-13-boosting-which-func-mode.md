---
title: Boosting Which Func Mode
---

[Which Func Mode](el-function:which-func-mode) shows the name of the current
“tag” in the mode line.  A tag is whatever the current major mode adds to its
[imenu](el-function:imenu) index, e.g. function names in Emacs Lisp, class
declarations in Python, or section names in Markdown.

By default, Which Func Mode just dumps the entire tag name at the very end of
the mode line, after the entire list of minor modes, which is neither
particularly sophisticated nor particularly visible.  When I
[redesigned my mode line][mode-line], I already moved it to a more prominent
place in my mode line, by changing the position of
[mode-line-misc-info](el-variable:mode-line-misc-info).

In this post I'll show you how to boost the tag name itself with some nifty
tricks.

<!--more-->

[mode-line]: internal:posts/make-your-emacs-mode-line-more-useful.md

Customising the appearance of Which Func Mode
=============================================

Which Func Mode exposes its mode line format in the variable
[which-func-format](el-variable:which-func-format), which holds a standard
[Mode Line Format][].  The default value takes the tag name from the internal
variable `which-func-current` and adds some standard text properties to specify
the key map (for mouse support) and faces.

We'll just copy the standard value, but replace `which-func-current` with our
own function:

```cl
(setq which-func-format
      `("["
        (:propertize (:eval (lunaryorn-which-func-current))
                     local-map ,which-func-keymap
                     face which-func
                     mouse-face mode-line-highlight
                     help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")
        "]")
      )
```

Note that we call our custom function `lunaryorn-which-func-format` to obtain
the actual tag name.  That's where the magic will happen.

[Mode Line Format]: http://www.gnu.org/s/emacs/manual/html_node/elisp/Mode-Line-Format.html

Truncating the tag name
=======================

Some IMenu tags can be really, really long.  Let's make some fun with nested
classes in Python:

```python
class Spam:

    class With:

        class Eggs:

            def bake(self):
                def doit():
                    ▮pass

            return doit
```

The box indicates the position of the point.  In this situation Which Func Mode
will show `Spam.With.Eggs.bake.doit` in the mode line.  That's a long name,
which takes a lot of space, especially on small displays.

Let's fix this, by truncating tag names to a maximum of 20 characters:

```cl
(require 'subr-x)

(defun lunaryorn-which-func-current ()
  (if-let (current (gethash (selected-window) which-func-table))
      (truncate-string-to-width current 20 nil nil "…")
    which-func-unknown))
```

This function takes the current tag name from `which-func-table` which caches
the current tag for each window, because computing the imenu index may be
expensive depending on the major mode and the buffer size.  If there is a
current tag, we truncate it to 20 characters at the end, replacing trailing text
with an ellipsis.  Otherwise we just return the standard “unknown” string.

We are using the `if-let` macro from `subr-x` in this function, which is only
available as of Emacs 24.4.  For earlier Emacs versions you can either replace
it with a nested `let`/`if`, or use the `-if-let` macro from the popular
[dash.el][] library.

We truncate the tag name at the end, because that's likely where the “nearest”
part of the tag name appears.  We have a good chance to see that in the buffer
anyway, so it's better to omit this part if we have little space, and preserve
“farther away” parts of the tag name.

[dash.el]: https://github.com/magnars/dash.el

Mode-specific truncation
========================

Of course, that's a little primitive and won't always yield good results.  We
can do better than that, by truncating with respect to the current major mode.

For instance, in Emacs Lisp we typically prefix global symbols with the name of
the defining library, as a poor man's namespace system.  Now, typically we know
what file we are in, and we can see the file name and the buffer name in the
mode line anyway, so it's really redundant in the tag name.  Let's remove it
from the tag name.

First we define a function to determine the current “namespace”, which returns
the buffer file name, if any, unless the file name refers to `init.el`.  In this
case we return the “namespace” used for functions in `init.el`, e.g. `lunaryorn`
in this example:

```cl
(defun lunaryorn-current-namespace ()
  "Determine the namespace of the current file."
  (when-let (filename (buffer-file-name))
    (if (string= (file-truename filename) (file-truename user-init-file))
        "lunaryorn"                       ; The “namespace” of my init
      (file-name-base filename))))
```

`when-let` is from `subr-x`, too, so everything said before about `if-let`
applies to it as well.  Notably, on Emacs 24.3 and earlier you need to replace
it with a nested `when`/`let` or with `-when-let` from [dash.el][].

With this function, we can now extend `lunaryorn-which-func-current`
accordingly:

```cl
(defun lunaryorn-which-func-current ()
  "Determine the name of the current function."
  (if-let (current (or (gethash (selected-window) which-func-table)))
      (truncate-string-to-width
       (pcase major-mode
         (`emacs-lisp-mode
          (let ((namespace (lunaryorn-current-namespace)))
            (if (and namespace
                     (string-prefix-p namespace current 'ignore-case))
                (concat "…" (substring current (length namespace)))
              current)))
         (_ current))
       20 nil nil "…")
    which-func-unknown))
```

We use `pcase` to dispatch on the `major-mode` of the current buffer.  In
`emacs-lisp-mode`, we obtain the namespace using our previously defined function
and remove it from the tag name.  For all other cases, we just return the tag
name without modifications.  The result is then truncated to 20 characters as
before.

<figure>
<img src="{{site.url}}{{site.baseurl}}/images/truncated-which-func-mode.png"
     alt="Mode line with truncated which-function for Python Mode"/>
</figure>

That's much better than the default.  And of course, my mode line features the
insanely awesome [nyan cat][], like any other decent folk's mode line does.

[nyan cat]: https://github.com/TeMPOraL/nyan-mode
