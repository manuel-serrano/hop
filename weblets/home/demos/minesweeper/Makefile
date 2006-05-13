#*=====================================================================*/
#*    serrano/prgm/project/hop/demos/minesweeper/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Mar  3 10:30:36 2006                          */
#*    Last change :  Tue Mar 21 09:25:12 2006 (serrano)                */
#*    Copyright   :  2006 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    Makefile of a HOP demo                                           */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

FILES      = minesweeper.hop minesweeper.scm \
             blank.gif bombquestion.gif revealed2.gif revealed6.gif \
	     bombdeath.gif bombrevealed.gif revealed3.gif revealed7.gif \
	     bombflagged.gif revealed0.gif revealed4.gif bombmisflagged.gif \
             revealed1.gif revealed5.gif
POPULATION = Makefile $(FILES)
DESTDIR    = $(HOPDEMOSDIR)/$(DEMO)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)
	install -m a+r $(POPULATION) $(DESTDIR)
