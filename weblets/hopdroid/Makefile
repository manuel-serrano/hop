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
SOURCES         = about.js apps.js auth.js config.js hopdroid.js \
                  phone.js privacy.js system.js webdav.js xml.js \
                  about-dark.hss privacy.hss about.hss system-dark.hss \
		  apps-dark.hss system.hss apps.hss webdav-dark.hss \
		  hopdroid-dark.hss webdav.hss hopdroid.hss xml-dark.hss \
		  privacy-dark.hss xml.hss \
                  box-arrow-up-right.svg grid-3x2-gap-fill.svg phone.svg \
		  box-seam.svg grid-3x3-gap.svg shield-lock.svg \
		  broadcast-pin.svg hdd-network.svg tag.svg \
		  calculator.svg hdd.svg toggle2-off.svg \
		  calendar2-check.svg house-door.svg toggle2-on.svg \
		  diagram-2-fill.svg info-circle.svg toggle-on.svg \
		  diagram-2.svg key.svg trash.svg \
		  exclamation-triangle-fill.svg lock.svg wifi.svg \
		  gear.svg phone-fill.svg x-octagon.svg
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
                     -e "s|@URLBASE@|$(HOPHOME)|" \
                     -e "s|@TITLE@|$(TITLE)|" \
                     -e "s|@MINHOP@|$(MINHOP)|" \
                     -e "s|@DATE@|$(DATE)|" \
                     -e "s|@HOST@|$(PRIVATEHOST)|" \
                     -e "s|@CATEGORY@|$(CATEGORY)|" > $@
