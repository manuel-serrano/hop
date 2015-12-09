${var hop = require( "hop" )}
${var hopdoc = require( "hopdoc" )}


### Emacs ###

The hopjs.el file contains various customizations and extensions of
the official Emacs js-mode.el for editing HopScript code. Its main features
are:

  1. Mixed JavaScript/HTML automatic indentation.
  2. On-line documentation access.
  3. Parenthesis/tag matching.
  4. Automatic closing context.


#### Installation ####

The file hopjs.el is installed into the directory:

    INSTALL_PREFIX/share/hop/site-lisp/hopjs.el

To use it, proceeds as follows:

 1. Add the INSTALLPREFIX/share/hop/site-lisp/hopjs.el to the Emacs
 load path. For that add the `$HOME/.emacs` file the following expression:
 
        (setq load-path (cons "INSTALLPREFIX/share/hop/site-lisp" load-path))

 2. Add the hopjs.el file in the _autoload_ of your emacs configuration.
 Append to `$HOME/.emacs` the following expression:
 
        (autoload 'hopjs-mode-hook "hopjs" "Hop.js javascript mode hook" t)

 3. Hook the hopjs.el facilities to the official js-mode.el:
 
        (add-hook 'js-mode-hook 'hopjs-mode-hook)

 4. Customize the js-mode indentation to conform the HopScript coding
 style:
 
        (custom-set-variables '(js-indent-level 3))


#### Key bindings ####

The hopjs.el mode defines the following key bindings:

  * `\[mouse-2\]`: on highlighted identifiers jumps to the on-line documentation.
  * `\[\C-c\C-c\]`: closes the opening context (_i.e._, a opening tag or a
  parenthesis).


#### Customization ####

The hopjs.el configuration can be access with:

    ESC-X: customize-group hopjs
