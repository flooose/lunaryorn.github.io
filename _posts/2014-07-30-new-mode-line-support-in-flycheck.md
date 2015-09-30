---
title: New Mode Line support in Flycheck
---

While [re-designing my Emacs mode line recently][mode-line], I realized that the
way [Flycheck][] used the mode line had been quite wrong all the time.

This post shows what was wrong in Flycheck, and how it was fixed.  I hope that
this helps other Emacs developers to avoid the same mistake.  I'll also show you
how my personal Flycheck mode line now looks like, to show you how proper mode
line support enables customizations that were simply impossible before.

<!--more-->

[mode-line]: internal:posts/make-your-emacs-mode-line-more-useful.md
[flycheck]: http://flycheck.readthedocs.org

Dynamic content in the mode line
================================

Like every other mode in Emacs, Flycheck has a mode line “lighter” that
indicates the mode in the mode line.  For most minor modes, the ligher is just a
simple string, for instance in [Emacs Lisp Slime Nav][]:

```cl
(define-minor-mode elisp-slime-nav-mode
  "Enable Slime-style navigation of elisp symbols using M-. and M-,"
  :init-value
  :lighter " SliNav"
  :keymap elisp-slime-nav-mode-map)
```

A constant mode line text isn't sufficient for Flycheck, however: Flycheck
should report the number of errors and warnings in the mode line, as well as the
general state of its syntax checking.  Fortunately, mode line lighters can also
be variables:

```cl
(define-minor-mode flycheck-mode
  "Minor mode for on-the-fly syntax checking."
  :init-value nil
  :keymap flycheck-mode-map
  :lighter flycheck-mode-line
  ;; …
  )
```

With this setup, Emacs will use the value of `flycheck-mode-line` as mode line
text for Flycheck Mode whenever it updates the mode line.

[Emacs Lisp Slime Nav]: https://github.com/purcell/elisp-slime-nav

Dynamic mode line done wrong
============================

In the past, `flycheck-mode-line` was a *buffer-local* variable, and Flycheck
would explicitly set this variable whenever the status of Flycheck in the
current buffer changed.  For instance, after a syntax check Flycheck called
`flycheck-report-error-count` with all errors in the current buffer to update
the mode line with the amount of errors and warnings:

```cl
(defun flycheck-report-status (status)
  "Report Flycheck STATUS."
  (setq flycheck-mode-line (concat flycheck-mode-line-lighter status))
  (force-mode-line-update))

(defun flycheck-report-error-count (errors)
  "Report ERRORS in the current buffer.

Report a proper flycheck status."
  (if errors
      (let ((error-counts (flycheck-count-errors errors)))
        (flycheck-report-status
         (format ":%s/%s"
                 (or (cdr (assq 'error error-counts)) 0)
                 (or (cdr (assq 'warning error-counts)) 0))))
    (flycheck-report-status "")))
```

As you can see, this code sets `flycheck-mode-line` to a *fixed* string.  There
is no way to customize the contents or appearance of Flycheck's mode line text,
or to disable mode line reporting completely.  There must be a better way, and
in fact there is.

Customizable mode line lighter
==============================

Mode line lighters can be more than just simple strings: In fact, they are valid
[mode line constructs][] on their own.  Mode line constructs are very versatile,
and can encode text properties, conditional content, and most notably, the
result of arbitrary expressions, via the `(:eval FORM)` construct.

Since commit [2d15110][] `flycheck-mode-line` is now a customizable mode line
construct, whose default value is an `(:eval FORM)` construct that creates the
same mode line text as before:

```cl
(defcustom flycheck-mode-line
  '(:eval (flycheck-mode-line-status-text))
  "Mode line lighter for Flycheck."
  :group 'flycheck
  :type 'sexp
  :risky t)
```

Flycheck *never* sets the value of this variable now.  Instead it just calls
`force-mode-line-update` whenever the status of Flycheck changes.  This causes
Emacs to re-draw the mode line of the corresponding buffer, which in turn
evaluates all mode line constructs, including the one in `flycheck-mode-line`.

The default value of `flycheck-mode-line` uses `(:eval FORM)` to call the new
function `flycheck-mode-line-status-text`, which returns a human-readable status
text for the current Flycheck status.  The functionality is still the same, but
unlike before, it's now entirely customizable and even be completely disabled:

```cl
(defun flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (let ((text (pcase (or status flycheck-last-status-change)
                (`not-checked "")
                (`no-checker "-")
                (`running "*")
                (`errored "!")
                (`finished
                 (if flycheck-current-errors
                     (let ((error-counts (flycheck-count-errors
                                          flycheck-current-errors)))
                       (format ":%s/%s"
                               (or (cdr (assq 'error error-counts)) 0)
                               (or (cdr (assq 'warning error-counts)) 0)))
                   ""))
                (`interrupted "-")
                (`suspicious "?"))))
    (concat " FlyC" text)))
```

[2d15110]: https://github.com/flycheck/flycheck/commit/2d1511012d7acbfc078decac0b08d7733bf954ae
[mode line constructs]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Data.html

My personal Flycheck mode line
==============================

To demonstrate what this enables you to do, I'd like to share my own mode line
setup:

```cl
(setq flycheck-mode-line
      '(:eval
        (pcase flycheck-last-status-change
          (`not-checked nil)
          (`no-checker (propertize " -" 'face 'warning))
          (`running (propertize " ✷" 'face 'success))
          (`errored (propertize " !" 'face 'error))
          (`finished
           (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                  (no-errors (cdr (assq 'error error-counts)))
                  (no-warnings (cdr (assq 'warning error-counts)))
                  (face (cond (no-errors 'error)
                              (no-warnings 'warning)
                              (t 'success))))
             (propertize (format " %s/%s" (or no-errors 0) (or no-warnings 0))
                         'face face)))
          (`interrupted " -")
          (`suspicious '(propertize " ?" 'face 'warning)))))
```

It uses mostly the same text as the standard value, but goes without the name of
the mode: Flycheck has a fixed place in my mode line, outside of the standard
minor mode list, and I don't need to see its name.  Unlike the standard value,
however, my setup makes heavy use of colors, to quickly give me an idea of the
buffer's state with just a single glance.

Feel free to use this as inspiration for your own setup, which may include all
sort of fancy stuff.  How about a little pirates flair by showing errors with ☠?
