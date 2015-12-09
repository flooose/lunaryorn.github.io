---
title: Configuring buffer display in Emacs
---

I guess every Emacs user knows this particular phenomenon: Windows constantly
pop up at almost, but not quite, entirely undesired places.  It is a
surprisingly hard challenge to make Emacs display buffers in a sane way.
Packages like winner and pop-win tell stories about the pain of generations of
Emacs users.  Well, it *used* to be a hard, but now it became much easier in
Emacs 24.1 with the new `display-buffer-alist` option.

<!--more-->

This option maps regular expressions for buffer names to actions which tell
Emacs how to display a buffer with a matching name.  This sounds a little
abstract at first, so let’s see this in action.  The following piece of code
adds a mapping for [Flycheck][]’s error list:

```cl
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))
```

Let’s dissect this:

1. The first item is a regular expression which matches the buffer name of
   Flycheck’s error list.  I use `rx` since I can never remember whether
   it’s ``\` `` or `\'` for the beginning of a string :)
2. Next follows a list of display functions.  Emacs tries these functions in the
   order of appearance to create a window for the buffer.
3. The remaining elements are cons cells of options to fine-tune the behaviour
   of the display functions.

In our example we start with `display-buffer-reuse-window` which will reuse an
existing window already showing the buffer: If the error list is already visible
in some window, there’s no need to show it twice.  By default, this function
only considers windows in the current frame, but with the option
`(reusable-frames . visible)` we extend its scope to all visible frames.
I don’t need to see the error list twice, if it’s already shown in another frame
on my secondary monitory.

If there’s no existing window with the error list Emacs will try the next
function, in our case `display-buffer-in-side-window`.  This function creates a
special “side” window for the error list.  A side windows always fixed to a
specific side of the frame and cannot be moved or splitted.  It behaves much
like the “dock windows” known from popular IDEs like IntelliJ or Visual Studio.

With the options `(side . bottom)` and `(height . 0.4)`,
`display-buffer-in-side-window` creates a side window at the bottom of the frame
with a height of 40% of the frame’s height.  If there’s already a side window at
the bottom of the current frame, `display-buffer-in-side-window` replaces the
existing side window with new side window for the error list.

In other words, Flycheck’s error list always pops up at the bottom of the
current frame now, occupying 40% of its height, just like error lists in Visual
Studio or IntelliJ.

I like to combine this feature with this little command:

```cl
(defun lunaryorn-quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))

(global-set-key (kbd "C-c q") #'lunaryorn-quit-bottom-side-windows)
```

Now I can press `C-c ! l` to show a list of all Flycheck errors at the bottom of
my frame, and `C-c q` to close it again :)

[Flycheck]: http://www.flycheck.org
