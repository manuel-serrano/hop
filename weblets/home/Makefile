#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/home/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Sat May 20 10:01:26 2006 (serrano)                */
#*    Copyright   :  2006 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

HOP_POP         = home.hop config.hop widgets.hop demo.hop wiki.hop \
                  presentation.wiki docs.wiki license.wiki faq.wiki \
                  download.wiki weblets.wiki install.wiki
POPULATION	= Makefile home.info .afile home.hss tips.hss \
                  demos icons articles etc/logo.png $(HOP_POP)

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
