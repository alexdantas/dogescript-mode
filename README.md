# dogescript-mode

Emacs major mode for editing [Dogescript source code][doge].

# Usage

Due to my lack of Emacs Lisp skills, this mode simply provides syntax highlight for
Dogescript keywords; nothing else.

An idea would be to fork `js-mode`, but I don't know that much Elisp.
Perhaps someone could help he with this?

# Installation

Put the file `dogescript-mode.el` inside your `.emacs.d/` load path and add the following
to your `.emacs` configuration file:

    (add-to-list 'auto-mode-alist '("\\.djs\\'" . dogescript-mode))

It will attach the dogescript mode to all files with the extension `.djs`.

# License

`dogescript-mode` is licensed under the GNU GPLv3.
See file `LICENSE.md` to see what you can and cannot do with the source code.

[doge]: https://github.com/remixz/dogescript

