#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/test/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Wed Nov 14 08:56:34 2007 (serrano)                */
#*    Copyright   :  2006-07 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the wizard weblet                                */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile test.hss test.hop \
                  image.hop withhop.hop event.hop \
                  fl.hop tree.hop notepad.hop paned.hop sorttable.hop \
                  tabslider.hop slider.hop canvas.hop \
                  busy.hop sound.hop audio.hop fselector.hop window.hop \
                  canvas-test.jpg sound-test.mp3 \
		  etc/weblet.info etc/test.wiki \
                  etc/logo.png

build: .afile

.afile: $(POPULATION)
	$(AFILE) -suffix hop $(POPULATION) > .afile

clean:
