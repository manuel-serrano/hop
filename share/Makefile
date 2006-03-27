#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/share/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 25 07:19:48 2004                          */
#*    Last change :  Mon Mar 27 14:01:21 2006 (serrano)                */
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
                  md5.js base64.js \
		  hop-lframe.css hop-slider.css hop-tabslider.css hop.js \
                  hop-notepad.css hop-slider.js hop-tabslider.js \
	 	  hop-notepad.js hop-sorttable.css hop-tree.css \
		  hop-foldlist.js hop-foldlist.css hop-window.js \
		  hop-window.css hop.scm \
		  icons Makefile

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop.scm $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop.css $(HOPSHAREDIR) && \
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
	install -m a+r $(BUILDSHAREDIR)/hop-window.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop-window.css $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/md5.js $(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/base64.js $(HOPSHAREDIR) && \
	mkdir -p $(HOPSHAREDIR)/icons && \
	chmod a+rx $(HOPSHAREDIR)/icons && \
	install -m a+r $(BUILDSHAREDIR)/icons/* $(HOPSHAREDIR)/icons

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	/bin/rm -f $(HOPSHAREDIR)/hop.scm && \
	/bin/rm -f $(HOPSHAREDIR)/hop.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop.css && \
	/bin/rm -f $(HOPSHAREDIR)/hop.hss && \
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
	/bin/rm -f $(HOPSHAREDIR)/hop-window.js && \
	/bin/rm -f $(HOPSHAREDIR)/hop-window.css && \
	/bin/rm -f $(HOPSHAREDIR)/base64.js && \
	/bin/rm -f $(HOPSHAREDIR)/md5.js && \
	/bin/rm -f $(HOPSHAREDIR)/icons


