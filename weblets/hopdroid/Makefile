#*=====================================================================*/
#*    serrano/prgm/project/hop/2.3.x/weblets/hop/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Thu May 31 08:51:11 2012 (serrano)                */
#*    Copyright   :  2006-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the HZ weblet                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = hopdroid
VERSION = $(HOPRELEASE)
MINHOP = $(HOPRELEASE)
MAXHOP =
CATEGORY = hop
DATE = $(HOPDATE)

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
SOURCES         = about.js apps.js hopdroid.js phone.js system.js xml.js
POPULATION	= Makefile package.json.in \
                  $(SOURCES) 

build: package.json

clean:

distclean:
	$(RM) package.json

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../etc/Makefile.hopconfig
-include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Dependencies                                                     */
#*---------------------------------------------------------------------*/

package.json: package.json.in Makefile
	cat $< | sed -e "s|@VERSION@|$(VERSION)|" \
                     -e "s|@HOPRELEASE@|$(HOPRELEASE)|" \
                     -e "s|@TITLE@|$(TITLE)|" \
                     -e "s|@MINHOP@|$(MINHOP)|" \
                     -e "s|@DATE@|$(DATE)|" \
                     -e "s|@HOST@|$(PRIVATEHOST)|" \
                     -e "s|@CATEGORY@|$(CATEGORY)|" > $@
