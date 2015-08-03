#*=====================================================================*/
#*    serrano/prgm/project/hop/3.0.x/doc/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 13:46:40 2006                          */
#*    Last change :  Mon Aug  3 14:28:49 2015 (serrano)                */
#*    Copyright   :  2006-15 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    ETC Makefile                                                     */
#*=====================================================================*/
do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
BOOTSTRAP_POP=css/bootstrap-theme.css css/bootstrap-theme.min.css \
  css/bootstrap.css css/bootstrap.min.css \
  js/bootstrap.js js/bootstrap.min.js  js/npm.js \
  fonts/glyphicons-halflings-regular.eot \
  fonts/glyphicons-halflings-regular.woff \
  fonts/glyphicons-halflings-regular.svg \
  fonts/glyphicons-halflings-regular.woff2 \
  fonts/glyphicons-halflings-regular.ttf

JQUERY_POP=js/jquery.min.js

POPULATION=$(BOOTSTRAP_POP:%=lib/bootstrap/%) $(JQUERY_POP:%=lib/jquery/%) \
  Makefile doc.js xml.js \
  doc.json \
  api/api.json api/00-hop.md \
  core/core.json core/html.bnf core/service.bnf core/tilde.bnf \
  core/00-syntax.md core/01-service.md core/02-worker.md \
  license.html hop.md \
  nodejs.md \
  hss/doc.hss

API_TARGETS=api.html 00-hop.html tree.html

CORE_TARGETS=core.html 00-syntax.html 01-service.html 02-worker.html

#*---------------------------------------------------------------------*/
#*    Search path                                                      */
#*---------------------------------------------------------------------*/
VPATH=api core ../node_modules/tree/doc

#*---------------------------------------------------------------------*/
#*    The hop executable                                               */
#*---------------------------------------------------------------------*/
.PHONY: build clean install uninstall

build: index.html license.html nodejs.html $(API_TARGETS) $(CORE_TARGETS)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	$(RM) $(API_TARGETS) $(CORE_TARGETS)
	$(RM) license.html index.html nodejs.html

devclean: clean

distclean: clean

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:

uninstall:

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES: .md .html .json

#*---------------------------------------------------------------------*/
#*    .md -> .html                                                     */
#*---------------------------------------------------------------------*/
%.html: %.md doc.js xml.js
	$(HOP) $(EFLAGS) --no-server -- $(BUILDDIR)/doc/doc.js $< > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    .json -> .html                                                   */
#*---------------------------------------------------------------------*/
%.html: %.json doc.js xml.js
	$(HOP) $(EFLAGS) --no-server -- $(BUILDDIR)/doc/doc.js $< > $@ \
          || ($(RM) $@; exit 1)

#*---------------------------------------------------------------------*/
#*    index.html ...                                                   */
#*---------------------------------------------------------------------*/
index.html: content.html doc.js xml.js hop.md
	$(HOP) $(EFLAGS) --no-server -- $(BUILDDIR)/doc/doc.js $< > $@ \
          || ($(RM) $@; exit 1)
