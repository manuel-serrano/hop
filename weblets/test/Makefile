#*=====================================================================*/
#*    serrano/prgm/project/hop/2.1.x/weblets/test/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Tue Apr 13 10:28:10 2010 (serrano)                */
#*    Copyright   :  2006-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the wizard weblet                                */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = test
VERSION = 1.0.0
MINHOP = $(HOPRELEASE) 
MAXHOP = $(HOPRELEASE)
CATEGORY = hop
DATE = $(HOPDATE)

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
SOURCES		= test.hop dom.hop \
                  image.hop withhop.hop event.hop hss.hop \
                  fl.hop tree.hop notepad.hop paned.hop sorttable.hop \
                  tabslider.hop slider.hop canvas.hop \
                  busy.hop audio.hop fselector.hop window.hop \
                  xhtml.hop svg.hop mathml.hop spinbutton.hop \
                  colorchooser.hop widget.hop gauge.hop

POPULATION	= Makefile test.hss hss.hss window.hss \
                  canvas-test.jpg \
                  sound-test.mp3 sound-test.ogg \
		  etc/weblet.info.in etc/test.wiki.in \
                  etc/logo.png etc/favicon.png etc/kangaroo.svgz \
                  $(SOURCES)

#*---------------------------------------------------------------------*/
#*    Targets                                                          */
#*---------------------------------------------------------------------*/
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
