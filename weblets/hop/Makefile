#*=====================================================================*/
#*    serrano/prgm/project/hop/2.3.x/weblets/hop/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Thu May 31 08:51:11 2012 (serrano)                */
#*    Copyright   :  2006-12 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = hop
VERSION = $(HOPRELEASE)
MINHOP = $(HOPRELEASE)
MAXHOP =
CATEGORY = hop
DATE = $(HOPDATE)

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
SOURCES         = hop.hop config.hop hop.hss
POPULATION	= Makefile \
                  etc/weblet.info.in etc/hop.wiki.in \
                  etc/logo.png etc/favicon.png etc/dashboard.png \
                  etc/default.png \
                  $(SOURCES) 

build: etc/weblet.info etc/$(TITLE).wiki .afile

.afile: $(SOURCES)
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

config.hop: config.hop.in Makefile
	cat $< | sed -e "s|@VERSION@|$(VERSION)|" \
                     -e "s|@HOPRELEASE@|$(HOPRELEASE)|" \
                     -e "s|@TITLE@|$(TITLE)|" \
                     -e "s|@MINHOP@|$(MINHOP)|" \
                     -e "s|@DATE@|$(DATE)|" \
                     -e "s|@HOST@|$(PRIVATEHOST)|" \
                     -e "s|@CATEGORY@|$(CATEGORY)|" > $@
