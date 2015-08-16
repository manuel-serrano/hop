#*=====================================================================*/
#*    serrano/prgm/project/hop/2.2.x/weblets/info/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Sat Dec 25 19:02:07 2010 (serrano)                */
#*    Copyright   :  2006-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Weblet Makefile                                                  */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = info
VERSION = 1.0.0
MINHOP = $(HOPRELEASE) 
MAXHOP =
CATEGORY = hop
DATE = $(HOPDATE)

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
SOURCES		= info.hop

POPULATION	= Makefile info.hss info.scm \
                  etc/weblet.info.in etc/info.wiki.in \
                  etc/logo.png etc/favicon.png \
                  etc/maze.scm \
                  $(SOURCES)

#*---------------------------------------------------------------------*/
#*    Targets                                                          */
#*---------------------------------------------------------------------*/
build: etc/weblet.info etc/$(TITLE).wiki .afile

.afile: $(SOURCES) etc/maze.scm
	$(AFILE) -suffix hop $(POPULATION) -o .afile

clean:

distclean:
	$(RM) etc/weblet.info etc/$(TITLE).wiki

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../etc/Makefile.hopconfig
-include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Dependencies                                                     */
#*---------------------------------------------------------------------*/
etc/weblet.info: etc/weblet.info.in Makefile
	cat $< | sed -e "s|@HZ@|$(HZ)|" \
                     -e "s|@TITLE@|$(TITLE)|" \
                     -e "s|@VERSION@|$(VERSION)|" \
                     -e "s|@MINHOP@|$(MINHOP)|" \
                     -e "s|@MAXHOP@|$(MAXHOP)|" \
                     -e "s|@PUBLISHER@|$(PUBLISHER)|" \
                     -e "s|@DATE@|$(DATE)|" \
                     -e "s|@PRIVATEHOST@|$(PRIVATEHOST)|" \
                     -e "s|@PUBLICHOST@|$(PUBLICKHOST)|" \
                     -e "s|@CATEGORY@|$(CATEGORY)|" > $@

%.wiki: %.wiki.in Makefile
	cat $< | sed -e "s|@VERSION@|$(VERSION)|" \
                     -e "s|@MINHOP@|$(MINHOP)|" \
                     -e "s|@HZ@|$(HZ)|" \
                     -e "s|@MAXHOP@|$(MAXHOP)|" \
                     -e "s|@HOST@|$(PRIVATEHOST)|" \
                     -e "s|@DATE@|$(DATE)|" \
                     -e "s|@PUBLISHER@|$(PUBLISHER)|" \
                     -e "s|@TITLE@|$(TITLE)|" \
                     -e "s|@CATEGORY@|$(CATEGORY)|" > $@
