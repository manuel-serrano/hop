#*=====================================================================*/
#*    .../hop/2.0.x/weblets/hzbuilder/etc/skeleton/Makefile            */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Fri Apr 17 13:40:46 2009 (serrano)                */
#*    Copyright   :  2006-09 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Generic Makefile to build Hop weblets.                           */
#*=====================================================================*/
## run "make" to build the .hz file

#*---------------------------------------------------------------------*/
#*    Weblet description                                               */
#*---------------------------------------------------------------------*/
HZ=@HZ@
VERSION=@VERSION@
CATEGORY=@CATEGORY@

MINHOP=@MINHOP@
MAXHOP=@MAXHOP@
WEBLETHOME=@HOME@
DOWNLOAD=@DOWNLOAD@

CLIENT_SOURCES=@HZ@.hop config.hop.in @HZ@.hss 
SOURCES=$(CLIENT_SOURCES)
DASHBOARDPNG=@DASHBOARDPNG@

FILES=$(SOURCES) \
  etc/logo.png etc/favicon.png etc/$(HZ).wiki.in etc/weblet.info.in \
  etc/homepage/01-homepage.wiki etc/homepage/01-homepage.png \
  $(DASHBOARDPNG) \
  Makefile

HOPREPOSITORY=@REPOSITORY@

#*---------------------------------------------------------------------*/
#*    Common configuration                                             */
#*---------------------------------------------------------------------*/
PUBLICHOST=http://hop.inria.fr
PRIVATEHOST=http://localhost:8080
OBJECTS=$(SOURCES:%.in=%)
PUBLISHER=http://hop.inria.fr/hop/weblets/synclist
DATE=$(shell date +'%d %B %Y')
AFILE=bglafile
BTAGS=bgltags

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES: .hz .wiki .hop .info

#*---------------------------------------------------------------------*/
#*    Specific rules                                                   */
#*---------------------------------------------------------------------*/
hz: $(HOPREPOSITORY)/$(HZ)-$(VERSION).hz

$(HOPREPOSITORY)/$(HZ)-$(VERSION).hz: .afile .etags \
                                      $(OBJECTS) \
                                      $(FILES) \
                                      etc/weblet.info \
                                      etc/$(HZ).wiki \
                                      etc/logo.png
	mkdir -p $(HOPREPOSITORY) && \
	(cd ..; tar cvfz $@ --exclude='$(HZ)/private' --exclude='*~' $(^:%=$(HZ)/%))

etc/weblet.info: etc/weblet.info.in Makefile
	cat $< | sed -e "s|@@HZ@@|$(HZ)|" \
                     -e "s|@@TITLE@@|$(TITLE)|" \
                     -e "s|@@VERSION@@|$(VERSION)|" \
                     -e "s|@@MINHOP@@|$(MINHOP)|" \
                     -e "s|@@MAXHOP@@|$(MAXHOP)|" \
                     -e "s|@@PUBLISHER@@|$(PUBLISHER)|" \
                     -e "s|@@DATE@@|$(DATE)|" \
                     -e "s|@@PRIVATEHOST@@|$(PRIVATEHOST)|" \
                     -e "s|@@PUBLICHOST@@|$(PUBLIKHOST)|" \
                     -e "s|@@HOME@@|$(WEBLETHOME)|" \
                     -e "s|@@DOWNLOAD@@|$(DOWNLOAD)|" \
                     -e "s|@@CATEGORY@@|$(CATEGORY)|" > $@

%.wiki: %.wiki.in Makefile
	cat $< | sed -e "s|@@VERSION@@|$(VERSION)|" \
                     -e "s|@@MINHOP@@|$(MINHOP)|" \
                     -e "s|@@HZ@@|$(HZ)|" \
                     -e "s|@@MAXHOP@@|$(MAXHOP)|" \
                     -e "s|@@HOST@@|$(PRIVATEHOST)|" \
                     -e "s|@@DATE@@|$(DATE)|" \
                     -e "s|@@PUBLISHER@@|$(PUBLISHER)|" \
                     -e "s|@@TITLE@@|$(TITLE)|" \
                     -e "s|@@PRIVATEHOST@@|$(PRIVATEHOST)|" \
                     -e "s|@@PUBLICHOST@@|$(PUBLIKHOST)|" \
                     -e "s|@@HOME@@|$(WEBLETHOME)|" \
                     -e "s|@@DOWNLOAD@@|$(DOWNLOAD)|" \
                     -e "s|@@CATEGORY@@|$(CATEGORY)|" > $@

config.hop: config.hop.in Makefile
	cat $< | sed -e "s|@@VERSION@@|$(VERSION)|" \
                     -e "s|@@TITLE@@|$(TITLE)|" \
                     -e "s|@@MINHOP@@|$(MINHOP)|" \
                     -e "s|@@DATE@@|$(DATE)|" \
                     -e "s|@@HOST@@|$(PRIVATEHOST)|" \
                     -e "s|@@URL@@|$(URL)|" \
                     -e "s|@@REPOSITORY@@|$(PUBLICREPOSITORY)|" \
                     -e "s|@@PRIVATEHOST@@|$(PRIVATEHOST)|" \
                     -e "s|@@PUBLICHOST@@|$(PUBLIKHOST)|" \
                     -e "s|@@HOME@@|$(WEBLETHOME)|" \
                     -e "s|@@DOWNLOAD@@|$(DOWNLOAD)|" \
                     -e "s|@@CATEGORY@@|$(CATEGORY)|" > $@

.afile: $(filter %.scm %.hop %.sch, $(OBJECTS))
	$(AFILE) -w -suffix hop $^ -o $@

.etags: $(filter %.scm %.hop %.sch, $(OBJECTS))
	$(BTAGS) --ignore-error -suffix hop --define-fun define-service $^ -o $@

.hop:
	echo -n "(" > $@
	for p in $(CLIENT_SOURCES:%.in=%); do \
          echo -n "\"$$p\" " >> $@; \
        done
	echo ")" >> $@

clean:
	/bin/rm -f etc/weblet.info
	/bin/rm -f .afile .etags .hop

getsources: 
	@echo $(filter %.scm %.hop %.sch, $(FILES))
