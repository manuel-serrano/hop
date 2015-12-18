#*=====================================================================*/
#*    serrano/prgm/project/hop/3.0.x/doc/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 13:46:40 2006                          */
#*    Last change :  Fri Dec 18 08:17:45 2015 (serrano)                */
#*    Copyright   :  2006-15 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    ETC Makefile                                                     */
#*=====================================================================*/
do: builddoc

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version

HOPFLAGS=-q --no-autoload --no-clear-cache --no-cache --no-server
HOP_MODULES=$(DESTDIR)$(HOPLIBDIR)/$(HOPFILDIR)/node_modules
HOP_LIBS_PATH=$(HOP_MODULES)/markdown/lib/.libs/$(HOPRELEASE)/$(MACH):$(HOP_MODULES)/fontifier/lib/.libs/$(HOPRELEASE)/$(MACH)
HOPBOOTFLAGS=--js-modules-dir $(BUILDDIR)/node_modules --icons-dir \
  $(BUILDDIR)/share/icons

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
BOOTSTRAP_POP=css/bootstrap.css css/bootstrap.min.css css/bootstrap.css.map \
  css/bootstrap-theme.css css/bootstrap-theme.min.css css/bootstrap-theme.css.map \
  js/bootstrap.js js/bootstrap.min.js  js/npm.js \
  fonts/glyphicons-halflings-regular.eot \
  fonts/glyphicons-halflings-regular.woff \
  fonts/glyphicons-halflings-regular.svg \
  fonts/glyphicons-halflings-regular.woff2 \
  fonts/glyphicons-halflings-regular.ttf

JQUERY_POP=js/jquery.min.js

POPULATION=$(BOOTSTRAP_POP:%=lib/bootstrap/%) $(JQUERY_POP:%=lib/jquery/%) \
  $(ICONS_POP:%/icons/%) \
  Makefile doc.js xml.js html.js ecma-262-60.js \
  doc.json \
  api/api.json api/00-hop.md api/10-dom.md api/20-websocket.md api/01-html.md \
  widget/widget.json \
  lang/lang.json _lang.md \
  lang/html.bnf lang/service.bnf lang/iservice.bnf \
  lang/tilde.bnf lang/syntax.bnf \
  lang/00-syntax.md \
  lang/01-es6.md lang/01-service.md lang/02-worker.md \
  lang/01-module.md \
  dev/dev.json dev/00-command.md dev/10-edit.md \
  license.md _index.md \
  nodejs.md download.md \
  hss/doc.hss \
  html-idx.json

API_TARGETS=api.html 00-hop.html 10-dom.html 20-websocket.html \
  hss.html config.html user.html 01-html.html markdown.html

LANG_TARGETS=lang.html 00-syntax.html 01-service.html 02-worker.html \
  01-es6.html 01-module.html

WIDGET_TARGETS=widget.html tree.html spage.html

DEV_TARGETS=dev.html 00-command.html 10-edit.html

ALL_TARGETS=index.html download.html nodejs.html \
  $(API_TARGETS) $(LANG_TARGETS) $(DEV_TARGETS) $(WIDGET_TARGETS) \
  idx.html

HTML="/usr/local/doc/html40/index/elements.html"

#*---------------------------------------------------------------------*/
#*    Search path                                                      */
#*---------------------------------------------------------------------*/
VPATH=api lang dev widget \
  ../node_modules/tree/doc \
  ../node_modules/spage/doc \
  ../node_modules/hss/doc \
  ../node_modules/config/doc \
  ../node_modules/user/doc \
  ../node_modules/markdown/doc

#*---------------------------------------------------------------------*/
#*    The hop executable                                               */
#*---------------------------------------------------------------------*/
.PHONY: build clean install uninstall

build:
	$(MAKE) builddoc HOPFLAGS="$(HOPFLAGS) $(HOPBOOTFLAGS)"

builddoc: $(ALL_TARGETS) \
  hss/markdown.css hss/fontifier.css hss/doc.css html-idx.json


#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	$(RM) $(API_TARGETS) $(LANG_TARGETS) $(WIDGET_TARGETS)
	$(RM) license.html index.html nodejs.html download.html dev.html
	$(RM) 00-command.html 10-edit.html
	$(RM) idx.json idx.html
	$(RM) -f hss/markdown.hss hss/fontifier.hss

devclean: clean

distclean: clean

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	$(MAKE) mkdir DIR=$(DESTDIR)$(HOPDOCDIR)
	$(MAKE) mkdir DIR=$(DESTDIR)$(HOPDOCDIR)/lib
	$(INSTALL) $(ALL_TARGETS) $(DESTDIR)$(HOPDOCDIR)
	$(INSTALL) idx.json $(DESTDIR)$(HOPDOCDIR)
	cp -r lib/bootstrap $(DESTDIR)$(HOPDOCDIR)/lib
	cp -r lib/jquery $(DESTDIR)$(HOPDOCDIR)/lib
	$(RM) -r -f $(DESTDIR)$(HOPDOCDIR)/hss
	cp -r hss $(DESTDIR)$(HOPDOCDIR)/hss
	$(INSTALL) doc.js $(DESTDIR)$(HOPDOCDIR)
	$(INSTALL) xml.js $(DESTDIR)$(HOPDOCDIR)
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/hss
	chmod $(MODFILE) $(DESTDIR)$(HOPDOCDIR)/hss/*
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib/bootstrap
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib/jquery
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib/bootstrap/css
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib/bootstrap/js
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib/bootstrap/fonts
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/lib/jquery/js
	chmod $(MODFILE) $(ALL_TARGETS:%=$(DESTDIR)$(HOPDOCDIR)/%)
	chmod $(MODFILE) $(JQUERY_POP:%=$(DESTDIR)$(HOPDOCDIR)/lib/jquery/%)
	chmod $(MODFILE) $(BOOTSTRAP_POP:%=$(DESTDIR)$(HOPDOCDIR)/lib/bootstrap/%)
	chmod $(MODFILE) $(DESTDIR)$(HOPDOCDIR)/lib/bootstrap/css/*
	$(INSTALL) html-idx.json $(DESTDIR)$(HOPDOCDIR)

uninstall:

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES: .md .html .json

#*---------------------------------------------------------------------*/
#*    .md -> .html                                                     */
#*---------------------------------------------------------------------*/
%.html: %.md doc.js xml.js
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          $(BUILDDIR)/doc/doc.js "compile-section" $< > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    .json -> .html                                                   */
#*---------------------------------------------------------------------*/
%.html: %.json doc.js xml.js
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          $(BUILDDIR)/doc/doc.js "compile-chapter" $< > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    index.html ...                                                   */
#*---------------------------------------------------------------------*/
index.html: _index.md doc.js xml.js
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          $(BUILDDIR)/doc/doc.js "compile-main" $< > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    idx.json ...                                                     */
#*---------------------------------------------------------------------*/
idx.json: $(API_TARGETS) $(LANG_TARGETS) $(DEV_TARGETS) $(WIDGET_TARGETS)
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          $(BUILDDIR)/doc/doc.js "html-to-idx" . $^ > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    idx.html ...                                                     */
#*---------------------------------------------------------------------*/
idx.html: idx.json
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          $(BUILDDIR)/doc/doc.js "compile-idx" $^ > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    html-idx.json ...                                                */
#*---------------------------------------------------------------------*/
html-idx.json: 
	$(HOP) $(HOPFLAGS) $(EFLAGS) -- \
          $(BUILDDIR)/doc/html.js $(HTML) > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    dependencies                                                     */
#*---------------------------------------------------------------------*/
download.html: license.md ../INSTALL.md
lang.html: _lang.md

hss/markdown.css: ../node_modules/markdown/hss/markdown.hss
	cp $< $@

hss/fontifier.css: ../node_modules/fontifier/hss/fontifier.hss
	cp $< $@

hss/doc.css: hss/doc.hss
	cp $< $@

