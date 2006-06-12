#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/home/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Mon Jun 12 12:00:06 2006 (eg)                     */
#*    Copyright   :  2006 Manuel Serrano                               */
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
                  download.wiki weblets.wiki install.wiki
POPULATION	= Makefile .afile home.hss tips.hss \
                  demos etc/icons articles etc/weblet-info etc/logo.png \
                  $(HOP_POP)

build: .afile license.wiki install.wiki release.info

.afile: $(HOP_POP)
	$(AFILE) -suffix hop $(POPULATION) demos/*/*.hop > .afile

license.wiki: ../../LICENSE
	cp $< $@

install.wiki: ../../INSTALL
	cp $< $@

release.info: ../../.hoprelease
	echo "(" > $@
	cat $< | awk -F= '{print " ("$$1 " " $$2")"}' >> $@
	echo ")" >> $@
