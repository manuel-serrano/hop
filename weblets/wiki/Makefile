#*=====================================================================*/
#*    serrano/prgm/project/hop/2.1.x/weblets/wiki/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Tue Mar  2 10:06:50 2010 (serrano)                */
#*    Copyright   :  2006-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the wiki weblet                                  */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = Wiki
VERSION = 1.2.0
MINHOP = $(HOPRELEASE) 
MAXHOP =
CATEGORY = hop
DATE = $(HOPDATE)

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
SOURCES	= wiki.hss wiki.hop wiki.scm config.hop.in wiki-prefs.hss
POPULATION = Makefile etc/prefs.png etc/doc.png etc/compile.png \
             wiki.scm \
             $(SOURCES)

build: etc/weblet.info etc/wiki.wiki config.hop .afile 

clean:

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../etc/Makefile.hopconfig
-include ../../etc/Makefile.version
include ../../etc/Makefile.weblet
