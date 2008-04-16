#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/weblets/home/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Wed Apr 16 10:34:23 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
#*=====================================================================*/
all: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

HOP_POP         = home.hop config.hop widgets.hop demo.hop wiki.hop \
                  presentation.wiki docs.wiki license.wiki faq.wiki \
                  download.wiki weblets.wiki install.wiki install-jvm.wiki \
                  community.wiki
POPULATION	= Makefile .afile home.hss tips.hss \
                  demos etc/icons articles etc/weblet.info etc/logo.png \
                  $(HOP_POP) config.hop.in

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
                 
build: .afile license.wiki install.wiki install-jvm.wiki release.info

.afile: $(HOP_POP) config.hop $(HOP_DEMOS:%=demos/%)
	$(AFILE) -suffix hop $^ -o $@

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

config.hop: config.hop.in Makefile
	cat $< | sed -e "s|@VERSION@|$(VERSION)|" \
                     -e "s|@TITLE@|$(TITLE)|" \
                     -e "s|@MINHOP@|$(MINHOP)|" \
                     -e "s|@DATE@|$(DATE)|" \
                     -e "s|@HOST@|$(PRIVATEHOST)|" \
                     -e "s|@CATEGORY@|$(CATEGORY)|" > $@


