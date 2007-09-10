#*=====================================================================*/
#*    serrano/prgm/project/hop/share/Makefile                          */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 25 07:19:48 2004                          */
#*    Last change :  Mon Sep 10 20:31:07 2007 (serrano)                */
#*    Copyright   :  2004-07 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP shared directory                        */
#*=====================================================================*/
.PHONY: do 

do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    FLASHOBJS                                                        */
#*---------------------------------------------------------------------*/
FLASHOBJS	= flash/HopServevt.swf

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= hop-paned.css hop-sorttable.js hop-tree.js \
		  hop-error.hss hop-paned.js hop-table.js hop.css \
                  hop.js hop-dom.js hop-fx.js \
                  md5.js base64.js \
		  hop-lframe.css hop-slider.css hop-tabslider.css \
                  hop-notepad.css hop-slider.js hop-tabslider.js \
	 	  hop-notepad.js hop-sorttable.css hop-tree.css \
		  hop-foldlist.js hop-foldlist.css \
                  hop-iwindow.js hop-iwindow.hss \
                  hop-editor.js hop-editor.css \
                  hop-fileselect.js hop-fileselect.css \
                  hop-tooltip.js hop-dashboard.js \
                  hopscheme.js \
                  hop-debug.js hop-debug.css \
		  hop-sound.js hop-sound.scm \
                  icons buttons editor flash Makefile \
		  $(FLASHOBJS) $(FLASHOBJS:%.swf=%.as) 

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
build: $(FLASHOBJS)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-autoconf.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop.css \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-dom.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-fx.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-error.hss \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-sorttable.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-sorttable.css \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-tree.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-tree.css \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-slider.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-slider.css \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-paned.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-paned.css \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-notepad.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-notepad.css \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-tabslider.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-tabslider.css \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-foldlist.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-foldlist.css \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-iwindow.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-iwindow.hss \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-editor.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-editor.css \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-fileselect.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-fileselect.css \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-debug.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-debug.css \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-sound.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-sound.scm \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/md5.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hopscheme.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/base64.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-tooltip.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-dashboard.js \
                       $(DESTDIR)$(HOPSHAREDIR) && \
	mkdir -p $(DESTDIR)$(HOPSHAREDIR)/icons && \
	chmod a+rx $(DESTDIR)$(HOPSHAREDIR)/icons && \
	install -m a+r $(BUILDSHAREDIR)/icons/* \
                       $(DESTDIR)$(HOPSHAREDIR)/icons && \
	mkdir -p $(DESTDIR)$(HOPSHAREDIR)/buttons && \
	chmod a+rx $(DESTDIR)$(HOPSHAREDIR)/buttons && \
	install -m a+r $(BUILDSHAREDIR)/buttons/* \
                       $(DESTDIR)$(HOPSHAREDIR)/buttons && \
	mkdir -p $(DESTDIR)$(HOPSHAREDIR)/editor && \
	chmod a+rx $(DESTDIR)$(HOPSHAREDIR)/editor && \
	install -m a+r $(BUILDSHAREDIR)/editor/* \
                       $(DESTDIR)$(HOPSHAREDIR)/editor && \
	mkdir -p $(DESTDIR)$(HOPSHAREDIR)/flash && \
	chmod a+rx $(DESTDIR)$(HOPSHAREDIR)/flash && \
	install -m a+r $(BUILDSHAREDIR)/flash/* \
                       $(DESTDIR)$(HOPSHAREDIR)/flash

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-autoconf.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop.css && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop.hss && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-dom.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-fx.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-sorttable.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-sorttable.css && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-tree.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-tree.css && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-slider.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-slider.css && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-paned.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-paned.css && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-notepad.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-notepad.css && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-tabslider.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-tabslider.css && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-foldlist.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-foldlist.css && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-iwindow.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-iwindow.hss && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-editor.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-editor.css && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-fileselect.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-fileselect.css && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-debug.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-debug.css && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/base64.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-dashboard.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hop-tooltip.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/hopscheme.js && \
	/bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/md5.js && \
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/icons && \
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/buttons && \
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/editor && \
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/flash

flash/%.swf: flash/%.as
	$(FLASHCC) $(FFLAGS) $@ $<
