#*=====================================================================*/
#*    serrano/prgm/project/hop/contribs/jsMath/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Dec  7 14:47:20 2005                          */
#*    Last change :  Wed Dec  7 16:14:16 2005 (serrano)                */
#*    Copyright   :  2005 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for the jsMath contribution                         */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig

#*---------------------------------------------------------------------*/
#*    Specific files                                                   */
#*---------------------------------------------------------------------*/
CONTRIB		= jsMath
CONTRIBSFILES	= jsMath.hop hop-jsMath.css jsMath demos

#*---------------------------------------------------------------------*/
#*    Contribs installation procedure                                  */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.contrib

#*---------------------------------------------------------------------*/
#*    Install/Uninstall                                                */
#*---------------------------------------------------------------------*/
install: install-contrib
	unzip -x jsMath-2.4b.zip -d $(CONTRIBSDIR)/$(CONTRIB)
	unzip -x jsMath-fonts-1.2.zip -d $(CONTRIBSDIR)/$(CONTRIB)/jsMath

uninstall: uninstall-contrib
