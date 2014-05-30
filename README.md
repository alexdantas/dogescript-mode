# dogescript-mode

![example](http://alexdantas.net/stuff/wp-content/uploads/2014/05/doge-js.png)

Emacs major mode for editing [Dogescript source code][doge].

# Usage

Due to my lack of Emacs Lisp skills, this mode simply provides syntax highlight for
Dogescript keywords; nothing else.

An idea would be to fork `js-mode`, but I don't know that much Elisp.
Perhaps someone could help he with this?

# Installation

Put the file `dogescript-mode.el` somewhere inside your `.emacs.d/` directory
and simply add the following line to your `.emacs` configuration file:

    (load "~/.emacs.d/path/to/dogescript-mode.el")

Now it will automatically launch `dogescript-mode` when you open files with the
`.djs` extension.

# License

`dogescript-mode` is licensed under the GNU GPLv3.
See file `LICENSE.md` to see what you can and cannot do with the source code.

[doge]: https://github.com/remixz/dogescript

