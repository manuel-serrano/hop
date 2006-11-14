#*=====================================================================*/
#*    serrano/prgm/project/hop/share/Makefile                          */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 25 07:19:48 2004                          */
#*    Last change :  Mon Nov 13 09:44:53 2006 (serrano)                */
#*    Copyright   :  2004-06 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP shared directory                        */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../etc/Makefile.hopconfig
include ../etc/Makefile.version

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
                  hop-iwindow.js hop-iwindow.css \
                  hop-editor.js hop-editor.css \
                  hop-fileselect.js hop-fileselect.css \
                  hopscheme.js \
                  hop-debug.js hop-debug.css \
		  hop-sound.js hop-sound.scm \
                  hop.scm icons buttons editor flash Makefile

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop.scm $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-autoconf.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-dom.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-fx.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-error.hss $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-sorttable.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-sorttable.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-tree.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-tree.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-slider.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-slider.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-paned.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-paned.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-notepad.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-notepad.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-tabslider.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-tabslider.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-foldlist.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-foldlist.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-iwindow.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-iwindow.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-editor.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-editor.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-fileselect.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-fileselect.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-debug.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-debug.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-sound.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-sound.scm $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/md5.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hopscheme.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/base64.js $(HOPSHAREDIR) && \
	mkdir -p $(HOPSHAREDIR)/icons && \
	chmod a+rx $(HOPSHAREDIR)/icons && \
	install -m a+r $(BUILDSHAREDIR)/icons/* $(HOPSHAREDIR)/icons && \
	mkdir -p $(HOPSHAREDIR)/buttons && \
	chmod a+rx $(HOPSHAREDIR)/buttons && \
	install -m a+r $(BUILDSHAREDIR)/buttons/* $(HOPSHAREDIR)/buttons && \
	mkdir -p $(HOPSHAREDIR)/editor && \
	chmod a+rx $(HOPSHAREDIR)/editor && \
	install -m a+r $(BUILDSHAREDIR)/editor/* $(HOPSHAREDIR)/editor && \
	mkdir -p $(HOPSHAREDIR)/flash && \
	chmod a+rx $(HOPSHAREDIR)/flash && \
	install -m a+r $(BUILDSHAREDIR)/flash/* $(HOPSHAREDIR)/flash

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	/bin/rm -f $(HOPSHAREDIR)/hop.scm && \
	/bin/rm -f $(HOPSHAREDIR)/hop-autoconf.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop.css && \
	/bin/rm -f $(HOPSHAREDIR)/hop.hss && \
	/bin/rm -f $(HOPSHAREDIR)/hop-dom.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-fx.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-sorttable.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-sorttable.css && \
	/bin/rm -f $(HOPSHAREDIR)/hop-tree.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-tree.css && \
	/bin/rm -f $(HOPSHAREDIR)/hop-slider.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-slider.css && \
	/bin/rm -f $(HOPSHAREDIR)/hop-paned.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-paned.css && \
	/bin/rm -f $(HOPSHAREDIR)/hop-notepad.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-notepad.css && \
	/bin/rm -f $(HOPSHAREDIR)/hop-tabslider.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-tabslider.css && \
	/bin/rm -f $(HOPSHAREDIR)/hop-foldlist.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-foldlist.css && \
	/bin/rm -f $(HOPSHAREDIR)/hop-iwindow.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-iwindow.css && \
	/bin/rm -f $(HOPSHAREDIR)/hop-editor.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-editor.css && \
	/bin/rm -f $(HOPSHAREDIR)/hop-fileselect.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-fileselect.css && \
	/bin/rm -f $(HOPSHAREDIR)/hop-debug.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-debug.css && \
	/bin/rm -f $(HOPSHAREDIR)/base64.js && \
	/bin/rm -f $(HOPSHAREDIR)/hopscheme.js && \
	/bin/rm -f $(HOPSHAREDIR)/md5.js && \
	/bin/rm -rf $(HOPSHAREDIR)/icons && \
	/bin/rm -rf $(HOPSHAREDIR)/buttons && \
	/bin/rm -rf $(HOPSHAREDIR)/editor && \
	/bin/rm -rf $(HOPSHAREDIR)/flash

