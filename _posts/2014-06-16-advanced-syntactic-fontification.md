---
title: Advanced syntactic fontification
category: emacs
tags: emacs-font-locking
layout: series-post
---

This article concludes my series about [Font Locking in Emacs][font-lock] by
illustrating how to hook into Emacs' syntactic analyses to implement
context-sensitive fontification.  If you are new to this series, you may want to
read the previous articles on [Syntactic Fontification in Emacs][syn-font-lock]
and [Search-based fontification with keywords][font-lock-keywords].

Context-sensitive fontification lets modes fontify text based on its syntactic
context.  In Puppet Mode we use this technique to fontify
[variable interpolations in double-quoted strings][variable interpolation].  In
this article we'll outline the implementation of this feature, which we
originally stole from the built-in Ruby Mode.

<!--more-->

[variable interpolation]: https://docs.puppetlabs.com/puppet/latest/reference/lang_datatypes.html#double-quoted-strings

Understanding the problem
=========================

For this feature we cannot just use the standard [font-lock-keywords][], because
we do not have access to the syntactic context during fontification.  We could
match the syntax of string interpolations with a corresponding regular
expression, but we cannot determine whether the interpolation occurred in a
double-quoted string or not, i.e. whether the interpolation is valid and should
be fontified or not.

To obtain this information, we need to hook into Emacs' syntactic analyses which
runs before fontification.  At this stage we have full access to Emacs' parser
state which tells us whether a given position in a buffer is inside a string, so
we can search for variable interpolations and determine whether they occur in a
double-quoted string or not.  We cannot immediately fontify interpolations,
though, simply because fontification has not yet started.

Instead, we need store the syntactic context in a text property at each variable
interpolation in a buffer.  In `font-lock-keywords` we can then access this text
property, and fontify valid interpolations accordingly.

[font-lock]: {% post_url 2014-03-12-font-locking-in-emacs %}
[spf]: el-variable:syntax-propertize-function
[flk]: el-variable:font-lock-keywords
[syn-font-lock]: {% post_url 2014-03-12-syntactic-fontification-in-emacs %}
[font-lock-keywords]: {% post_url 2014-03-26-search-based-fontification-with-keywords %}

Hooking into syntactic analyses
===============================

To hook into syntactic analyses, we use a custom
[`syntax-propertize-function`][spf].  This function is actually intended to let
major modes put [Syntax Properties][] onto text, in order to override and refine
the static classification from the syntax table, but it can be used to run
arbitrary code during syntactic analyses as well.

We register our custom function in the major mode definition:

```cl
(define-derived-mode puppet-mode prog-mode "Puppet" ()
  ;; …
  (set (make-local-variable 'syntax-propertize-function)
       #'puppet-syntax-propertize-function)
  ;; …
)
```

`puppet-syntax-propertize-function` will now be called during syntactic
analyses.  In this function we search for variable interpolations in the buffer:

```cl
(defun puppet-syntax-propertize-function (start end)
  (let ((case-fold-search nil))
    (goto-char start)
    (remove-text-properties start end '(puppet-interpolation))
    (funcall
     (syntax-propertize-rules
      ((rx (or line-start (not (any "\\")))
           (zero-or-more "\\\\")
           (group "${"
                  (optional "::")
                  (zero-or-more (any "a-z")
                                (zero-or-more
                                 (any "A-Z" "a-z" "0-9" "_"))
                                "::")
                  (one-or-more (any "A-Z" "a-z" "0-9" "_")) "}"))
       (0 (ignore (puppet-syntax-propertize-interpolation)))))
     start end)))
```

Since variable names are case-sensitive in Puppet, we ensure that our regular
expression is case-sensitive as well by `let`-binding `case-fold-start`
accordingly.  Then we navigate to the start of the region being analysed, and
clear state that we stored in previous analyses, by removing all occurrences of
the `puppet-interpolation` property.

Eventually we search for variable interpolations in the region being analysed.
We conveniently avoid the tedious way of calling `re-search-forward` in a
`while` loop and instead abuse a little utility macro named
[`syntax-propertize-rules`][spr][^1].  At the first glance, this looks a little
strange.

[`syntax-propertize-rules`][spr] does not do any work on its own, but instead
*generates* and returns at macro expansion time a function that processes a
buffer according to the given rules.  Hence, we need to make an explicit
function call by placing a `funcall` expression around
`syntax-propertize-rules`.

The argument to `syntax-propertize-rules` is a list of rules, each consisting of
a regular expression to search for, and a Emacs Lisp expression supposed to
return the value for the `syntax-table` text property of the matched text.

However, we are not actually interested in changing this property.  Our rule
serves a different purpose: We want process each occurrence of a variable
interpolation to inspect and store the syntactic context for later use during
fontification.

Hence, our rule searches for variable interpolations and simply calls another
function named `puppet-syntax-propertize-interpolation`, carefully placing an
`ignore` around the function call to avoid that the return value of the function
accidentally leaks into the `syntax-table` text property, thus messing up
syntactic analyses.

Like in our [previous post][font-lock-keywords] we use [`rx`][rx] to write
readable regular expressions.  The expression reflects a simplified variant of
[variable interpolation][].  It does not match all possible syntactic variants
of interpolations, but fur the purpose of this post it is totally sufficient.
The expression actually used in Puppet Mode is a little more intricate though.

[Syntax Properties]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Properties.html#Syntax-Properties
[spr]: el-function:syntax-propertize-rules
[rx]: el-function:rx

Inspecting the syntactic context
================================

`puppet-syntax-propertize-interpolation` will now called during syntactic
analyses, for each occurrence of a variable interpolation throughout the buffer.
In this function, we have access to the syntactic context *and* to the match
data of our regular expression, and can proceed to inspect the syntactic
context:

```cl
(defun puppet-syntax-propertize-interpolation ()
  (let* ((beg (match-beginning 0))
         (context (save-excursion (save-match-data (syntax-ppss beg)))))
    (put-text-property beg (1+ beg) 'puppet-interpolation
                       (cons (nth 3 context) (match-data)))))
```

We take the relevant part of syntactic context at the beginning of the matched
variable interpolation, and store it in the `puppet-interpolation` text
property, together with the original match data, for later use during
fontification.

[`syntax-ppss`][sppss] gives us the entire syntactic context, as a
[fairly intricate list][parser state] with various properties obtained from
analysing the current buffer with the current syntax table.  For our purposes,
we just need to know whether the interpolation is inside a string and what kind
of string it occurred in.  This information is available at the 4th element of
the syntactic context, which holds the delimiter character of the string the
given position is in, or `nil` if the position is outside of a string.

Since `syntax-ppss` can move the point and modify the match data, we carefully
wrap the call in `save-excursion` and `save-match-data` respectively, to avoid
tainting the global state.

By storing the match data along with the string information, we make the
boundaries of the interpolation available during fontification.

[sppss]: el-function:syntax-ppss
[parser state]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Parser-State.html

Fontifying based on the syntactic context
=========================================

To fontify the variable interpolations that we found during syntactic analyses,
we make use of a special feature of [`font-lock-keywords`][flk].  In the
[previous article][font-lock-keywords] we used font lock keywords with regular
expressions, but font lock keywords may also use a function to match syntax.

Building on the [setup of the previous article][setup], we add a font lock
keyword to match variable interpolations.  We fontify valid variable
interpolations, i.e. those that occur inside double-quoted strings, as
variables:

```cl
(defvar puppet-font-lock-keywords
  `(
    ;; …
    (puppet-match-interpolation 0 font-lock-variable-name-face t)
    ;; …
    ))
```

The keyword starts with the *matcher function*, followed the group which to
assign the face to, the face name, and a flag indicating that previous
fontification should be overwritten.  The latter is needed in our case, since we
match interpolations *inside* strings, which were already fontified by syntactic
fontification.  Without this special flag, fontification would simply ignore the
string contents completely.

A matcher function shall search for the next occurrence of the corresponding
syntax construct, move the point and set the match data accordingly.  Hence, we
stored the match data in the previous section.  If it found a match, it should
return non-nil, and `nil` otherwise.  Font Lock Mode calls the matcher function
repeatedly until it returns `nil` and fontifies all matches.

In our matcher function `puppet-match-interpolation` we try to find the variable
interpolations again, and restore the match data from the stored state to enable
fontification:

```cl
(defun puppet-match-interpolation (limit)
  (let ((pos (next-single-char-property-change (point) 'puppet-interpolation
                                               nil limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((value (get-text-property pos 'puppet-interpolation)))
        (if (eq (car value) ?\")
            (progn
              (set-match-data (cdr value))
              t)
          (puppet-match-interpolation limit))))))
```

Instead of applying the same regular expression for interpolations again, we
scan for occurrences of your special `puppet-interpolation` text property.  Any
such occurrence marks a variable interpolation that we found during syntactic
analyses.

If we found an occurrence, we move the point to it and extract the value of the
property.  Remember that the value is a cons cell `(string-delimiter
. match-data)`.  By looking at `string-delimiter`, we can determine whether the
interpolation occurs inside a double-quoted string.  If that is the case, we
restore the saved `match-data` and return `t` to indicate that our matcher
function found a match.

Otherwise the variable interpolation occurred at no valid position, so we
recursively continue to search for the next occurrence, until we hit the
`limit`.  If `next-single-char-property-change` reaches `limit` without finding
an occurrence of the property, it returns `nil`, causing our function return
`nil` as well.

[setup]: {% post_url 2014-03-26-search-based-fontification-with-keywords %}#setup-boilerplate

Debugging
=========

The technique that we presented in this posting is not easy to debug, since it
works with in-buffer state (i.e. text properties) that is not immediately
visible.

Fortunately, Emacs provides us with a tool to make it visible:
[`describe-char`][dc], at <kbd>C-u C-x =</kbd>, which pops up a buffer showing
the syntax classification and the text properties of the character at point.
With this tool, we can now verify that our code indeed works, by checking the
text properties:

<figure>
<img src="{{site.url}}{{site.baseurl}}/images/describe-char.png"
     alt="describe-char buffer shown text properties"/>
</figure>

We see that our `puppet-interpolation` property exists (by pressing the `Show`
button we could also check its value), and that the character was fontified with
`font-lock-variable-name-face`, like we defined in `font-lock-keywords`.

[dc]: el-function:describe-char

Conclusion
==========

In this post we demonstrated an advanced approach to Emacs' font locking, that
opens up for almost infinite possibilities.  In Puppet Mode, we use this
technique not only for variable interpolations in double-quoted strings, but
also for regular expression literals and escape sequences in strings, that would
be hard to fontify correctly with `font-lock-keywords` only.

With this article, my series about fontification in Emacs is now at its end.  I
hope that it helps major mode authors to make their modes shiny, and gives
interested users the knowledge and motivation to start their own major modes or
contribute to Emacs projects.

So long

[^1]: As in our previous posts, the actual implementation in Puppet Mode is much
      more intricate.  In Puppet Mode we do context-sensitive fontification for
      other syntax as well, and we also need to place Syntax Properties at some
      places to account for multi-character operators, so the use of
      `syntax-propertize-rules` is justified by more than simply convenience.
