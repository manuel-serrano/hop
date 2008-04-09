#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/weblets/wiki/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Fri Apr  4 10:47:35 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the wiki weblet                                  */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = Wiki
VERSION = 1.1.0
MINHOP = $(HOPRELEASE) 
MAXHOP =

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION = Makefile wiki.hss wiki.hop config.hop.in \
             etc/weblet.info.in etc/wiki.wiki.in \
             etc/logo.png etc/favicon.png

build: etc/weblet.info etc/wiki.wiki config.hop .afile 

.afile: $(filter %.scm %.hop %.sch, $(POPULATION)) config.hop
	$(AFILE) -suffix hop $^ -o $@

clean:

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

config.hop: config.hop.in Makefile
	cat $< | sed -e "s|@VERSION@|$(VERSION)|" \
                     -e "s|@TITLE@|$(TITLE)|" \
                     -e "s|@MINHOP@|$(MINHOP)|" \
                     -e "s|@DATE@|$(DATE)|" \
                     -e "s|@HOST@|$(PRIVATEHOST)|" \
                     -e "s|@CATEGORY@|$(CATEGORY)|" > $@


