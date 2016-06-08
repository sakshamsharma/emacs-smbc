Emacs-SMBC
==========

The fun (and the copyright) credits go to [SMBC](http://smbc-comics.com/) of course!

Emacs-SMBC is an Emacs plugin to help you (Save the world? Sadly no) read SMBC without leaving your Emacs!

It is planned to have a lot of functions to make reading SMBC easier.

### Usage
Simply load the file (place it in your `.emacs.d` and load it, or else use `M-x load-file` and give its path).<br>
Better still, add it to your `init.el`, it's easy and fast to load.

Access the latest comic with `M-x smbc-get-latest`.

### Misc
On MELPA as `smbc` [here](https://github.com/sakshamsharma/melpa/blob/master/recipes/smbc)
Reddit discussion [here](https://www.reddit.com/r/emacs/comments/4lrjln/a_simple_smbc_viewer_for_emacs/)

### TODO

* Check for new comics every time on startup
* Allow reading previous comics as well
* Cache last few comics
* Allow opening comic in browser
* Use RSS feed instead of parsing HTML
* Read some more SMBC :smile:
