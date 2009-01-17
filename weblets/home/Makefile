#*=====================================================================*/
#*    serrano/prgm/project/hop/1.11.x/weblets/home/Makefile            */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Thu Jan 15 18:52:12 2009 (serrano)                */
#*    Copyright   :  2006-09 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
#*=====================================================================*/
all: build

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = home
VERSION = $(HOPRELEASE) 
MINHOP = $(HOPRELEASE) 
MAXHOP = $(HOPRELEASE)
CATEGORY = hop
DATE = $(HOPDATE)

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
SOURCES         = home.hop config.hop widgets.hop demo.hop wiki.hop
HOP_POP		= presentation.wiki docs.wiki license.wiki faq.wiki \
                  download.wiki weblets.wiki install.wiki install-jvm.wiki \
                  community.wiki videos.wiki
VIDEOS		= hopx10.flv
POPULATION	= Makefile .afile home.hss tips.hss \
                  demos etc/icons articles etc/weblet.info.in etc/home.wiki.in \
                  etc/logo.png etc/favicon.png etc/video.png \
                  $(VIDEOS:%=videos/%) \
                  $(SOURCES) $(HOP_POP) config.hop.in

HOP_DEMOS	= minesweeper/minesweeper.hop \
                  music/music.hop \
                  repl/repl.hop \
                  rewrite/rewrite.hop \
                  rss/rss.hop \
                  set/set.hop \
                  set/util.hop \
                  set/game.hop \
                  source/source.hop \
                  sudoku/sudoku.hop \
                  wiki/wiki.hop

                 
build: .afile license.wiki install.wiki install-jvm.wiki release.info \
       config.hop \
       etc/weblet.info etc/home.wiki

clean:
	$(RM) config.hop

distclean:
	$(RM) etc/weblet.info etc/$(TITLE).wiki

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

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

license.wiki: ../../LICENSE
	cp $< $@

install.wiki: ../../INSTALL
	cp $< $@

install-jvm.wiki: ../../INSTALL.jvm
	cp $< $@

release.info: ../../.hoprelease
	echo "(" > $@
	cat $< | awk -F= '{print " ("$$1 " " $$2")"}' >> $@
	echo ")" >> $@

.afile: $(SOURCES) config.hop $(HOP_DEMOS:%=demos/%)
	$(AFILE) -suffix hop $^ -o $@
