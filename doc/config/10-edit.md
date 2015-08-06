${var hop = require( "hop" )}
${var hopdoc = require( "hopdoc" )}


### Emacs ###

The `hopjs.el` file contains various customizations and extensions of
the official Emacs `js-mode.el` for editing HopScript code. The file
is installed into the directory:

    INSTALL_PREFIX/share/hop/site-lisp/hopjs.el

To use it, proceeds as follows:

 1. Add the INSTALLPREFIX/share/hop/site-lisp/hopjs.el to the Emacs
 load path. For that add the `$HOME/.emacs` file the following expression:
 
        (setq load-path (cons "INSTALLPREFIX/share/hop/site-lisp" load-path))

 2. Add the `hopjs.el` file in the _autoload_ of your emacs configuration.
 Append to `$HOME/.emacs` the following expression:
 
        (autoload 'hopjs-mode-hook "hopjs" "Hop.js javascript mode hook" t)

 3. Hook the `hopjs.el` facilities to the official `js-mode.el`:
 
        (add-hook 'js-mode-hook 'hopjs-mode-hook)

 4. Customize the js-mode indentation to conform the HopScript coding
 style:
 
        (custom-set-variables '(js-indent-level 3))

