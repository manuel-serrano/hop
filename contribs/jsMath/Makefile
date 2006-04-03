#*=====================================================================*/
#*    serrano/prgm/project/hop/contribs/jsMath/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Dec  7 14:47:20 2005                          */
#*    Last change :  Mon Apr  3 15:01:52 2006 (serrano)                */
#*    Copyright   :  2005-06 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile for the jsMath contribution                         */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Specific files                                                   */
#*---------------------------------------------------------------------*/
CONTRIB		= jsMath
CONTRIBSFILES	= README jsMath.hop hop-jsMath.css demos \
                  jsMath-2.4b.zip jsMath-fonts-1.2.zip

#*---------------------------------------------------------------------*/
#*    Contribs installation procedure                                  */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.contrib

#*---------------------------------------------------------------------*/
#*    Install/Uninstall                                                */
#*---------------------------------------------------------------------*/
install: install-contrib
	unzip -x jsMath-2.4b.zip -d $(HOPCONTRIBSDIR)/$(CONTRIB)
	unzip -x jsMath-fonts-1.2.zip -d $(HOPCONTRIBSDIR)/$(CONTRIB)/jsMath

uninstall: uninstall-contrib

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile $(CONTRIBSFILES)
