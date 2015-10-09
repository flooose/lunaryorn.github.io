---
title: Ansible Documention in Emacs
twittercard: summary_large_image
twittercardimage: /images/ansible-doc.png
---

**Update** *(October 2, 2014)*: This feature is now available in the
[ansible-doc][] package.

[ansible-doc]: https://github.com/lunaryorn/ansible-doc.el

----

In [Flycheck][] I'm using [Ansible][] to setup a testing environment on
[Travis CI][] or on a local [Vagrant][] virtual machine.  I do not particularly
like Ansible, and still much prefer Puppet, but it turned out to be twice as
fast as Puppet, and much easier to use for occasional contributors.

One nice feature of Ansible, though, is the `ansible-doc` command which provides
offline documentation for all available Ansible modules.  With some lines of
Emacs Lisp we can conveniently use this command to view Ansible documentation
from within Emacs:

<!--more-->

```cl
(defvar lunaryorn-ansible-modules nil
  "Cache of all known Ansible modules.")

(defun lunaryorn-ansible-modules ()
  "Get a list of all known Ansible modules."
  (unless lunaryorn-ansible-modules
    (let ((lines (ignore-errors (process-lines "ansible-doc" "--list")))
          modules)
      (dolist (line lines)
        (push (car (split-string line (rx (one-or-more space)))) modules))
      (setq lunaryorn-ansible-modules (sort modules #'string<))))
  lunaryorn-ansible-modules)

(defconst lunaryorn-ansible-doc-buffer " *Ansible Doc*"
  "The Ansible Doc buffer.")

(defun lunaryorn-ansible-doc (module)
  "Show ansible doc for MODULE."
  (interactive
   (list (ido-completing-read "Ansible Module: "
                              (lunaryorn-ansible-modules)
                              nil nil nil nil
                              (thing-at-point 'symbol 'no-properties))))
  (let ((buffer (get-buffer-create lunaryorn-ansible-doc-buffer)))
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (view-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (call-process "ansible-doc" nil t t module))
      (goto-char (point-min)))
    (display-buffer buffer)))
```

`lunaryorn-ansible-doc` prompts for the name of an Ansible module, defaulting to
the module at point, looks up the documentation with `ansible-doc`, and displays
the resulting buffer.  `lunaryorn-ansible-modules` obtains a list of all modules
to provide IDO-based completion in the prompt.  Since `ansible-doc --list` is
quite slow, the list of modules is cached in a variable.

Let's bind this new command to a key:

```cl
(eval-after-load 'yaml-mode
  '(define-key yaml-mode-map (kbd "C-c h a") #'lunaryorn-ansible-doc))
```

Now we can conveniently lookup documentation with <kbd>C-c h a</kbd> in a YAML
Mode buffer:

<figure>
<img src="/images/ansible-doc.png"
     alt="Documentation of ansible apt module in Emacs buffer"/>
</figure>

In case you wonder, why the IDO prompt is vertical, that's
[IDO Vertical Mode][].

[Flycheck]: http://www.flycheck.org
[Ansible]: http://www.ansible.com
[Travis CI]: http://travis-ci.org
[Vagrant]: http://vagrantup.com
[IDO Vertical Mode]: https://github.com/gempesaw/ido-vertical-mode.el
