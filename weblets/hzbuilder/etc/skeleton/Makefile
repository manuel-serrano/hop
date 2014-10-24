#*=====================================================================*/
#*    .../hop/3.0.x/weblets/hzbuilder/etc/skeleton/Makefile            */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Fri Oct 24 15:37:03 2014 (serrano)                */
#*    Copyright   :  2006-14 Manuel Serrano                            */
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
LICENSE=@LICENSE@

MINHOP=@MINHOP@
MAXHOP=@MAXHOP@
WEBLETHOME=@HOME@
DOWNLOAD=@DOWNLOAD@
PUBLISHER=@PUBLISHER@

CLIENT_SOURCES=@HZ@@SUFFIX@ config@SUFFIX@.in @HZ@.hss 
SOURCES=$(CLIENT_SOURCES)
DASHBOARDPNG=@DASHBOARDPNG@

FILES=$(SOURCES) \
  etc/logo.png etc/favicon.png etc/$(HZ).wiki.in @CONFIG@.in \
  etc/doc.png etc/prefs.png \
  etc/homepage/01-homepage.wiki etc/homepage/01-homepage.png \
  $(DASHBOARDPNG) \
  Makefile

HOPREPOSITORY=@REPOSITORY@

#*---------------------------------------------------------------------*/
#*    Common configuration                                             */
#*---------------------------------------------------------------------*/
OBJECTS=$(SOURCES:%.in=%)
PUBLISHER=http://hop.inria.fr/hop/weblets/synclist
DATE=$(shell date +'%d %B %Y')
AFILE=bglafile
BTAGS=bgltags

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES: .hz .wiki @SUFFIX@ .info

#*---------------------------------------------------------------------*/
#*    Specific rules                                                   */
#*---------------------------------------------------------------------*/
hz: $(HOPREPOSITORY)/$(HZ)-$(VERSION).hz

$(HOPREPOSITORY)/$(HZ)-$(VERSION).hz: .afile .etags \
                                      $(OBJECTS) \
                                      $(FILES) \
                                      @CONFIG@ \
                                      etc/$(HZ).wiki \
                                      etc/logo.png
	mkdir -p $(HOPREPOSITORY) && \
	(cd ..; tar cvfz $@ --exclude='$(HZ)/private' --exclude='*~' $(^:%=$(HZ)/%))

@CONFIG@: @CONFIG@.in Makefile
	cat $< | sed -e "s|@@HZ@@|$(HZ)|" \
                     -e "s|@@TITLE@@|$(TITLE)|" \
                     -e "s|@@VERSION@@|$(VERSION)|" \
                     -e "s|@@MINHOP@@|$(MINHOP)|" \
                     -e "s|@@MAXHOP@@|$(MAXHOP)|" \
                     -e "s|@@PUBLISHER@@|$(PUBLISHER)|" \
                     -e "s|@@DATE@@|$(DATE)|" \
                     -e "s|@@HOME@@|$(HOME)|" \
                     -e "s|@@DOWNLOAD@@|$(DOWNLOAD)|" \
                     -e "s|@@PUBLISHER@@|$(PUBLISHER)|" \
                     -e "s|@@CATEGORY@@|$(CATEGORY)|" \
                     -e "s|@@LICENSE@@|$(LICENSE)|" > $@

package.json: package.json.in Makefile
	cat $< | sed -e "s|@@HZ@@|$(HZ)|" \
                     -e "s|@@TITLE@@|$(TITLE)|" \
                     -e "s|@@VERSION@@|$(VERSION)|" \
                     -e "s|@@MINHOP@@|$(MINHOP)|" \
                     -e "s|@@MAXHOP@@|$(MAXHOP)|" \
                     -e "s|@@PUBLISHER@@|$(PUBLISHER)|" \
                     -e "s|@@DATE@@|$(DATE)|" \
                     -e "s|@@HOME@@|$(HOME)|" \
                     -e "s|@@DOWNLOAD@@|$(DOWNLOAD)|" \
                     -e "s|@@PUBLISHER@@|$(PUBLISHER)|" \
                     -e "s|@@CATEGORY@@|$(CATEGORY)|" \
                     -e "s|@@LICENSE@@|$(LICENSE)|" > $@

%.wiki: %.wiki.in Makefile
	cat $< | sed -e "s|@@VERSION@@|$(VERSION)|" \
                     -e "s|@@MINHOP@@|$(MINHOP)|" \
                     -e "s|@@HZ@@|$(HZ)|" \
                     -e "s|@@MAXHOP@@|$(MAXHOP)|" \
                     -e "s|@@DATE@@|$(DATE)|" \
                     -e "s|@@TITLE@@|$(TITLE)|" \
                     -e "s|@@HOME@@|$(WEBLETHOME)|" \
                     -e "s|@@DOWNLOAD@@|$(DOWNLOAD)|" \
                     -e "s|@@PUBLISHER@@|$(PUBLISHER)|" \
                     -e "s|@@CATEGORY@@|$(CATEGORY)|" \
                     -e "s|@@LICENSE@@|$(LICENSE)|" > $@

config@SUFFIX@: config@SUFFIX@.in Makefile
	cat $< | sed -e "s|@@VERSION@@|$(VERSION)|" \
                     -e "s|@@TITLE@@|$(TITLE)|" \
                     -e "s|@@MINHOP@@|$(MINHOP)|" \
                     -e "s|@@DATE@@|$(DATE)|" \
                     -e "s|@@URL@@|$(URL)|" \
                     -e "s|@@HOME@@|$(WEBLETHOME)|" \
                     -e "s|@@DOWNLOAD@@|$(DOWNLOAD)|" \
                     -e "s|@@PUBLISHER@@|$(PUBLISHER)|" \
                     -e "s|@@CATEGORY@@|$(CATEGORY)|" \
                     -e "s|@@LICENSE@@|$(LICENSE)|" > $@

.afile: $(filter %.scm %@SUFFIX@ %.sch, $(OBJECTS))
	$(AFILE) -w -suffix hop $^ -o $@

.etags: $(filter %.scm %@SUFFIX@ %.sch, $(OBJECTS))
	$(BTAGS) --ignore-error -suffix hop --define-fun define-service $^ -o $@

@SUFFIX@:
	echo -n "(" > $@
	for p in $(CLIENT_SOURCES:%.in=%); do \
          echo -n "\"$$p\" " >> $@; \
        done
	echo ")" >> $@

clean:
	$(RM) -f @CONFIG@
	$(RM) -f package.json
	$(RM) -f .afile .etags @SUFFIX@

getsources: 
	@echo $(filter %.scm %.hop %.sch %.js, $(FILES))
