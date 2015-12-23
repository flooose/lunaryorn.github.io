---
title: Generic syntax checkers in Flycheck
category: emacs
---

In two years of maintaining Flycheck I received many wishes and feature
requests.  I implemented many, discarded some, but I never came around to add
the most requested feature and fix the [oldest issue still open][#169]: Check a
buffer with an arbitrary Emacs Lisp function.

Now, after more than one year of waiting, this feature is finally there:
Flycheck now supports “generic syntax checkers”, which call synchronous or
asynchronous Emacs Lisp functions instead of invoking external commands.  They
are “generic” because they are essentially a superset of normal syntax checkers:
In fact, regular syntax checkers are now called “command syntax checkers” and
implemented on top of this new feature, as a specific kind of generic checkers.

<!--more-->

For users of Flycheck nothing has changed other than that you'll probably see a
lot of new syntax checkers emerging in the next weeks and months.

For developers, however, this opens many new interesting possibilities.  You
have even more power to write syntax checkers for your favourite languages now:
Even if there is no linting tool or if it's not feasible to use an external
tool, you can now write syntax checkers using a persistent background process or
a running REPL instead.  You could even write lints entirely in Emacs Lisp,
although that's not really recommendable due to Emacs synchronous execution[^1].

For instance, Flycheck never had an OCaml syntax checker.  Checking OCaml is not
as easy as checking C, because dependency management is more sophisticated and
more complicated in OCaml.  Luckily, there is a tool called [Merlin][] which
does all the dirty work, providing a persistent background process that caches
the entire state of an OCaml project.  It's mainly used for completion, but it
can also check a buffer for errors.

With generic syntax checkers, we can finally implement a syntax checker that
talks to a Merlin process to check a buffer for errors[^2].  This post explains
the implementation of this syntax checker (available in the [flycheck-ocaml][]
extension) to introduce generic syntax checkers to you.

Note that the Emacs Lisp code in this article is written for lexical scoping,
and will not work with dynamical scoping.  Please keep that in mind when
writing your own syntax checkers.

[#169]: https://github.com/flycheck/flycheck/issues/169
[Merlin]: https://github.com/the-lambda-church/merlin
[flycheck-ocaml]: https://github.com/flycheck/flycheck-ocaml

Defining a generic syntax checker
=================================

The new function [flycheck-define-generic-checker][fdgc] defines a “generic”
syntax checker.  A generic syntax checker looks almost like a regular syntax
checker, except there is a `:start` function instead of a `:command` and
`:error-patterns`[^3]:

```cl
(flycheck-define-generic-checker 'ocaml-merlin
  "A syntax checker for OCaml using Merlin Mode.

See URL `https://github.com/the-lambda-church/merlin'."
  :start #'flycheck-ocaml-merlin-start
  :modes '(caml-mode tuareg-mode)
  :predicate (lambda () merlin-mode))
```

Like a regular command syntax checker, a generic checker needs `:modes` and
(optionally) a `:predicate`.  In this case we specify the common OCaml editing
modes and an additional predicate that checks whether `merlin-mode`–which
provides the background process we are going to use for syntax checking—is
active.

All of this is just the standard boilerplate for syntax checkers, but `:start`
is a new thing.  Here the fun begins:  The value of `:start` is a *function* to
start the syntax check.

[fdgc]: http://www.flycheck.org/en/latest/dev/api.html#el.function.flycheck-define-generic-checker

Starting a syntax check
=======================

Flycheck calls this `:start` function whenever it needs to check the buffer,
just like it would invoke the `:command` of a command syntax checker.  The
`:start` function takes two arguments:

1. The syntax checker being run, just in case the `:start` function is being
   shared among different syntax checkers.
2. A callback function, which shall be used to report the results of a syntax
   check back to Flycheck.

The `flycheck-ocaml-merlin-start` function of our new syntax checker looks as
follows:

```cl
(defun flycheck-ocaml-merlin-start (checker callback)
  "Start a Merlin syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."
  (merlin-sync-to-point (point-max) t)
  ;; Put the current buffer into the closure environment so that we have access
  ;; to it later.
  (let ((buffer (current-buffer)))
    (merlin-send-command-async
     'errors
     (lambda (data)
       (condition-case err
           (let ((errors (mapcar
                          (lambda (alist)
                            (flycheck-ocaml-merlin-parse-error alist checker
                                                               buffer))
                          data)))
             (funcall callback 'finished (delq nil errors)))
         (error (funcall callback 'errored (error-message-string err)))))
     ;; The error callback
     (lambda (msg) (funcall callback 'errored msg)))))
```

At the beginning we update Merlin with the current contents of the buffer.  This
step still happens synchronously, but the subsequent error checking is entirely
asynchronous.  `merlin-send-command-async` sends the `errors` command to the
Merlin background process.  When finished Merlin calls either of the two
callbacks given to this function: The first callback with the results of a
successful check, and the second with an error message if the check failed.

We provide two `lambda`s for these callbacks: The first callback parses the
Merlin result and turns it into a proper Flycheck result, whereas the second
callback just forwards the error message to Flycheck.

Note that we put the current buffer into a local variable.  Since the command is
executed asynchronously, we can't use `(current-buffer)` in the Merlin callback:
The current buffer might have changed meanwhile.  By putting it in a local
variable we permanently store the buffer being checked in the closure
environment.

Reporting results
=================

Let use focus on the second callback—the error callback—first, for the sake of
simplicity.  This callback just calls Flycheck's own callback with two
arguments:

1. The *status symbol*
2. The *status metadata*

These are the two components of the [“status protocol”][status] that Flycheck
provides for generic syntax checkers to communicate with Flycheck.

The status symbol `errored` tells Flycheck that an error occurred in the syntax
check.  Its metadata is simply the corresponding error message as string.

The other important status symbol is `finished`, which tells Flycheck that a
syntax check as properly finished.  It's metadata is a (potentially empty) list
of [flycheck-error][] objects which denote errors in the current buffer.

[status]: http://www.flycheck.org/en/latest/dev/api.html#status-callback-protocol
[flycheck-error]: http://www.flycheck.org/en/latest/dev/api.html#el.struct.flycheck-error

Parsing errors
==============

Before we can report errors, though, we need to parse them.  This is what we do
in the first `merlin-send-command-async` callback.

Merlin returns the list of errors as a list of alists in the `data` argument to
the callback.  Each alist describes a single error in the buffer.  We map over
all alists with `flycheck-ocaml-merlin-parse-error` to turn them into
`flycheck-error` objects.  Eventually we pass these parsed errors to Flycheck's
`callback`, using the `finished` status.  This causes Flycheck to report these
errors in the buffer, just like it does for regular command syntax checkers.

`flycheck-ocaml-merlin-parse-error` extracts the relevant keys from each error
list, and creates a new `flycheck-error` object:

```cl
(defun flycheck-ocaml-merlin-parse-error (alist checker buffer)
  "Parse a Merlin error ALIST from CHECKER in BUFFER into a `flycheck-error'.

Return the corresponding `flycheck-error'."
  (let* ((orig-message (cdr (assq 'message alist)))
         (start (cdr (assq 'start alist)))
         (line (or (cdr (assq 'line start)) 1))
         (column (cdr (assq 'col start))))
    (when orig-message
      (pcase-let* ((`(,level . ,message)
                    (flycheck-ocaml-merlin-parse-message orig-message)))
        (if level
            (flycheck-error-new-at line column level message
                                   :checker checker
                                   :buffer buffer
                                   :filename (buffer-file-name))
          (lwarn 'flycheck-ocaml :error
                 "Failed to parse Merlin error message %S from %S"
                 orig-message alist)
          nil)))))
```

`flycheck-ocaml-merlin-parse-message` applies a regular expression to the
original error messages in order to extract the error level (which Merlin does
not report otherwise) and the real error message.  It's rather dumb and not
particularly interesting, so I omitted it for the sake of brevity.  If you are
interested, take a look at the Github page of [flycheck-ocaml][].

`flycheck-error-new-at` then creates and returns a new `flycheck-error` object
with the information obtained from Merlin.  Note that you really need to fill
the *entire* structure, except for those slots which are explicitly marked
optional in the [documentation][flycheck-error].  Notably, Flycheck needs the
`:checker`, `:buffer` and `:filename` slots to associate the error object with
the right buffer.

And that's it!  With just these few lines of code we have defined a syntax
checker that talks to a persistent background process to check the current
buffer, and parses the results reported by the background process into a format
understood by Flycheck.

Why did this take so long
=========================

If it's that simple, why did it take so long, I hear you ask.  Well, internally
things aren't that simple.

Since Emacs lacks any asynchronous features other than processes, Flycheck grew
based on external processes until it had entirely absorbed them.  Breaking this
close relationship with processes required a lot of refactoring, which is a
tedious and error-prone task in a language with as weak a type system as Emacs
Lisp.

With the lack of asynchronous functions also comes a notorious lack of modern
concurrency primitives in Emacs, which makes it hard to design a reasonable
generic API for an asynchronous task.  Where I'd just have used a future or
channel in a decent programming language, Flycheck does a lot of internal
book-keeping of its current state via global variables.

Consequently there are no means to properly debug concurrent code either.  A
race condition in the new process management code that resulted from the
refactoring took me days to debug, until I could finally isolate it with
literally dozens of `(message "DEBUG")` calls all over the place.

Final words
===========

Nonetheless, I'm sorry that it took so long, and I hope that what I have come up
with compensates for the year that you have all waited.  I took quite some time
to debug and test the new API and the refactored internals of Flycheck, so there
should be little to no bugs left, but if you find any please do not hesitate to
report them to the [issue tracker][].

And if you choose to try the new API and write one or another syntax checker
yourself: I'm really keen on your feedback!  Please tell me whether you like the
API, whether the documentation was sufficient, whether you had any difficulties,
and—most importantly—what you think can be improved.

[^1]: Essentially, an Emacs Lisp linter would freeze Emacs while checking the
      buffer, which is not particularly convenient.

[^2]: The same technique can be used for other languages, which Flycheck does
      not yet sufficiently support.  For instance, there's no support for Java
      or Clojure in Flycheck because of the long JVM startup times.  A generic
      syntax checker however could send a Clojure buffer of to a running Clojure
      REPL, which is much faster than starting a new Java process.

[^3]: Also, unlike `flycheck-define-checker` the syntax checker name and the
      property values must be quoted, because `flycheck-define-generic-checker`
      is really a function and not a macro.

[issue tracker]: https://github.com/flycheck/flycheck/issues
