#*=====================================================================*/
#*    serrano/prgm/project/hop/demos/Makefile                          */
#*    -------------------------------------------------------------    */
#*    Author      :  Erick Gallesio                                    */
#*    Creation    :  Wed Mar  1 14:25:06 2006                          */
#*    Last change :  Sat May  6 14:48:30 2006 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP demos directory                         */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../etc/Makefile.hopconfig
include ../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    POPDIRS                                                          */
#*---------------------------------------------------------------------*/
POPDIRS = calendar dhtml event filtering foldlist \
	  hss notepad paned slider sorttable \
	  tabslider text todolist tree minesweeper \
	  sudoku wikiedit

FILES   = list.hop

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile $(FILES)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(HOPDEMOSDIR)
	install -m a+r $(FILES) $(HOPDEMOSDIR)
	for d in $(POPDIRS) ;do \
          $(MAKE) -C $$d install DEMO=$$d ; \
	done

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	/bin/rm -rf $(HOPDEMOSDIR)
