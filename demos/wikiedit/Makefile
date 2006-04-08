#*=====================================================================*/
#*    serrano/prgm/project/hop/demos/wikiedit/Makefile                 */
#*    -------------------------------------------------------------    */
#*    Author      :  Erick Gallesio                                    */
#*    Creation    :  Wed Mar 22 22:25:21 2006                          */
#*    Last change :  Sat Apr  8 07:10:38 2006 (serrano)                */
#*    -------------------------------------------------------------    */
#*    Makefile of a HOP demo                                           */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

FILES      = wikiedit.hop wikiedit.hss
POPULATION = Makefile $(FILES)
DESTDIR    = $(HOPDEMOSDIR)/$(DEMO)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)
	install -m a+r $(POPULATION) $(DESTDIR)
