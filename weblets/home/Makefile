#*=====================================================================*/
#*    serrano/prgm/project/hop/3.0.x/weblets/home/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Wed Dec  9 08:19:02 2015 (serrano)                */
#*    Copyright   :  2006-15 Manuel Serrano                            */
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
SOURCES         = home.hop widgets.hop demo.hop wiki.hop
HOP_POP		= presentation.wiki docs.wiki license.wiki faq.wiki \
                  download.wiki weblets.wiki install-jvm.wiki \
                  vm.wiki android.wiki community.wiki videos.wiki
VIDEOS		= # hopx10.ogv hopx10.flv

TALKS		= hopdiffuse.xhtml.gz

ARTICLES	= hss/article.xhtml semantics/article.pdf

POPULATION	= Makefile home.hss tips.hss \
                  demos etc/icons articles etc/weblet.info.in etc/home.wiki.in \
                  etc/logo.png etc/favicon.png etc/video.png \
                  etc/icons/maemo_install.png etc/icons/android-market.png \
                  etc/icons/vlinux.png \
                  $(VIDEOS:%=videos/%) \
                  $(TALKS:%=talks/%) \
                  $(ARTICLES:%=articles/%) \
                  $(SOURCES) $(HOP_POP) config.hop.in

HOP_DEMOS	= minesweeper/minesweeper.hop \
	          minesweeper/minesweeper.scm \
                  music/music.hop music/music.hss \
                  video/video.hop video/video.hss \
                  rewrite/rewrite.hop rewrite/rewrite.hss \
                  rss/rss.hop rss/rss.hss \
                  set/set.hop set/set.scm set/util.hop set/game.hop \
	          popart/popart.hop popart/popart.hss \
	          map/map.hop map/map.hss \
                  source/source.hop source/source.hss \
                  sudoku/sudoku.hop sudoku/sudoku.scm \
                  wiki/wiki.hop wiki/wiki.hss

                 
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

config.hop: config.hop.in Makefile ../../etc/Makefile.hopconfig
	cat $< | sed -e "s|@VERSION@|$(VERSION)|" \
                     -e "s|@HOPRELEASE@|$(HOPRELEASE)|" \
                     -e "s|@TITLE@|$(TITLE)|" \
                     -e "s|@MINHOP@|$(MINHOP)|" \
                     -e "s|@DATE@|$(DATE)|" \
                     -e "s|@HOST@|$(PRIVATEHOST)|" \
                     -e "s|@CATEGORY@|$(CATEGORY)|" > $@

license.wiki: ../../LICENSE
	cp $< $@

install.wiki: ../../INSTALL.md
	cp $< $@

install-jvm.wiki: ../../INSTALL.jvm
	cp $< $@

release.info: ../../.hoprelease
	echo "(" > $@
	cat $< | awk -F= '{print " ("$$1 " " $$2")"}' >> $@
	echo ")" >> $@

.afile: $(SOURCES) config.hop $(HOP_DEMOS:%=demos/%)
	$(AFILE) -suffix hop $^ -o $@
