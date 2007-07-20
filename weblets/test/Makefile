#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/test/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Fri Jul 20 11:49:29 2007 (serrano)                */
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
                  image.hop event.hop \
                  fl.hop tree.hop notepad.hop paned.hop sorttable.hop \
                  slider.hop canvas.hop \
                  busy.hop sound.hop fselector.hop \
                  canvas-test.jpg sound-test.mp3 \
		  etc/weblet.info etc/test.wiki \
                  etc/logo.png

build: .afile

.afile: $(POPULATION)
	$(AFILE) -suffix hop $(POPULATION) > .afile

clean:
