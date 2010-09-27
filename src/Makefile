#*=====================================================================*/
#*    serrano/prgm/project/hop/2.2.x/src/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  serrano                                           */
#*    Creation    :  Fri Feb 18 17:48:08 2000                          */
#*    Last change :  Mon Sep 27 12:20:06 2010 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The Makefile that builds Hop                                     */
#*=====================================================================*/
do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version
-include $(BIGLOOLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    Target and Project                                               */
#*---------------------------------------------------------------------*/
EXEC		= hop

#*---------------------------------------------------------------------*/
#*    Sources    					               */
#*---------------------------------------------------------------------*/
MODULES		= hop_param parseargs main init \
                  scheduler accept pipeline \
                  nothread_scheduler \
                  queue_scheduler \
                  oto_scheduler \
                  pool_scheduler \
                  amany_scheduler

INCLUDES	= stage.sch

CLASS_PATH	= o/class_s
CLASS_DIR	= $(CLASS_PATH)/bigloo/hop

OBJECTS 	= $(MODULES:%=o/%.o)
BGL_CLASSES	= $(MODULES:%=$(CLASS_DIR)/%.class)

SOURCES 	= $(MODULES:%=%.scm)

COLOR		= 32

STATICLIBS	= $(BUILDLIBDIR)/libhop_s-$(HOPRELEASE).a \
                  $(BUILDLIBDIR)/libhop_es-$(HOPRELEASE).a \
                  $(BUILDLIBDIR)/libhopscheme_s-$(HOPRELEASE).a \
                  $(BUILDLIBDIR)/libhopscheme_es-$(HOPRELEASE).a \
                  $(BUILDLIBDIR)/libscheme2js_s-$(HOPRELEASE).a \
                  $(BUILDLIBDIR)/libscheme2js_es-$(HOPRELEASE).a \
                  $(BUILDLIBDIR)/libhopwidget_s-$(HOPRELEASE).a \
                  $(BUILDLIBDIR)/libhopwidget_es-$(HOPRELEASE).a \
                  $(BIGLOOLIBDIR)/libbiglooweb_s-$(RELEASE).a \
                  $(BIGLOOLIBDIR)/libbiglooweb_es-$(RELEASE).a \
                  $(BIGLOOLIBDIR)/libbigloomultimedia_s-$(RELEASE).a \
                  $(BIGLOOLIBDIR)/libbigloomultimedia_es-$(RELEASE).a \
                  $(BIGLOOLIBDIR)/libbigloopthread_s-$(RELEASE).a \
                  $(BIGLOOLIBDIR)/libbigloopthread_es-$(RELEASE).a

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION 	= $(INCLUDES) $(SOURCES) Makefile

#*---------------------------------------------------------------------*/
#*    The hop executable                                               */
#*---------------------------------------------------------------------*/
.PHONY: build dobuild link.static link.dynamic link.library

build:  o $(BACKEND)

native: $(BUILDBINDIR)/$(EXEC)
jvm: $(CLASS_DIR) $(BUILDBINDIR)/$(EXEC).jar

o: 
	mkdir -p $@

$(CLASS_DIR):
	mkdir -p $@

$(BUILDBINDIR)/$(EXEC): .afile .etags $(OBJECTS)
	$(MAKE) link.$(LINK) DEST=$@
	@ $(call done,$(COLOR),"Binary \($(LINK)\) ",$@," done.")

link.dynamic:
	$(call compile2,BIGLOO,BCFLAGS,BCFLAGSDEV, -o $(DEST) $(OBJECTS))

link.static:
	$(call compile2,BIGLOO,BCFLAGS,BCFLAGSDEV, -o $(DEST) $(OBJECTS) $(STATICLIBS))

link.library:
	echo "***ERROR: link.library not currently supported!"
	exit 1

# if we build for Apache Felix we must add the 3 java wrappers 
# needed to run hop from a bundle
# FELIX_JAR = 
#    /home/mdione/src/works/inria/orange/felix-framework-2.0.4/bin/felix.jar
ifdef FELIX_JAR
FELIX_MODULES = HopRunner HopThread HopActivator
else
FELIX_MODULES =
endif
FELIX_SOURCES = $(FELIX_MODULES:%=%.java)
FELIX_CLASSES = $(FELIX_MODULES:%=$(CLASS_DIR)/%.class)

$(BUILDBINDIR)/$(EXEC).jar: .afile .etags .jfile $(BGL_CLASSES) jvm-stdlibs jvm-share jvm-etc jvm-lib $(FELIX_CLASSES)
	@ $(JAR) $@ META-INF/MANIFEST.MF -C o/class_s .
	@ $(call done,$(COLOR),"Binary ",$@,"...done.")

jvm-stdlibs: o/class_s/lib/hop/$(HOPRELEASE)
	for p in $(JVMZIPLIBS); do \
          if [ -f $$p ]; then \
             $(UNZIP) -d o/class_s -o -u $$p; \
          else \
             echo "Warning: Can't find zip file $$p..."; \
          fi; \
        done
	for p in $(JVMINITS); do \
          if [ -f $$p ]; then \
             cp $$p o/class_s/lib/hop/$(HOPRELEASE); \
          else \
             echo "Warning: Can't find init file $$p..."; \
          fi; \
        done

jvm-share:
	$(RM) -r o/class_s/share
	(cd ../share && $(MAKE) hop.js) && cp -r ../share o/class_s
	cp -r ../scheme2js/runtime/*.js o/class_s/share

jvm-etc:
	mkdir -p o/class_s/etc
	cp ../etc/hoprc.hop o/class_s/etc
	$(MAKE) -s jvm-list SRC=.. DIR=etc DEST=o/class_s

jvm-list:
	ls $(SRC)/$(DIR) > $(DEST)/$(DIR)/.list
	for p in `ls $(SRC)/$(DIR)`; do \
	  if [ -d $(SRC)/$(DIR)/$$p ]; then \
            $(MAKE) jvm-list SRC=$(SRC)/$(DIR) DIR=$$p DEST=$(DEST)/$(DIR) || (echo "*** ERROR: $$p"; exit 1); \
          fi \
        done

jvm-lib: o/class_s/lib/hop/$(HOPRELEASE)
	cp -r ../weblets o/class_s/lib
	$(MAKE) -s jvm-list SRC=.. DIR=weblets DEST=o/class_s/lib
	cp -f $(BUILDLIBDIR)/*.init $<


o/class_s/lib/hop/$(HOPRELEASE):
	mkdir -p $@

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .o

#*---------------------------------------------------------------------*/
#*    The implicit `.scm.o' rule                                       */
#*---------------------------------------------------------------------*/
o/%.o: %.scm .afile
	@ $(call compile,BIGLOO,BCFLAGS,$< -o $@ -c)

$(CLASS_DIR)/%.class: %.scm .afile
	@ $(call compile,BIGLOO,BJVMFLAGS,$< -o $@ -c)

$(CLASS_DIR)/%.class: %.java
	/usr/lib/jvm/java-1.5.0-sun-1.5.0.17/bin/javac -classpath $(CLASS_PATH):$(FELIX_JAR) -d $(CLASS_PATH) $<

o/main.o: main.scm
	@ $(call compile2,BIGLOO,BCFLAGS,BCFLAGSDEV,$< -o $@ -c)

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags dep

.afile: $(SOURCES)
	@ $(AFILE) -o .afile $(SOURCES) 

.jfile: $(SOURCES)
	@ $(JFILE) -o .jfile -pbase bigloo.hop $(SOURCES)

.etags: .afile
	@ $(BTAGS) -o .etags $(SOURCES)

getsources:
	@ echo $(SOURCES)

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
.PHONY: dep
dep: 
	@(num=`grep -n '^#bdepend start' Makefile | awk -F: '{ print $$1}' -`;\
          if [ "$$num " != " " ]; then \
            head -n `expr $$num - 1` Makefile > /tmp/Makefile.aux; \
          else \
            cp Makefile /tmp/Makefile.aux; \
          fi; \
          head -n `expr $$num - 1` Makefile > /tmp/Makefile.aux)
	@ $(DEPEND) -strict-obj-dir o -fno-mco $(SOURCES) >> /tmp/Makefile.aux
	@ mv /tmp/Makefile.aux Makefile

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: install-$(BACKEND)

install-native:
	$(INSTALL) $(BUILDBINDIR)/$(EXEC) $(DESTDIR)$(HOPBINDIR)/hop-$(HOPRELEASE)
	chmod $(MODEXE) $(DESTDIR)$(HOPBINDIR)/hop-$(HOPRELEASE)
	(cd $(DESTDIR)$(HOPBINDIR) && $(RM) hop && ln hop-$(HOPRELEASE) hop)

install-jvm:
	$(INSTALL) $(BUILDBINDIR)/$(EXEC).jar $(DESTDIR)$(HOPBINDIR)
	chmod $(MODFILE) $(DESTDIR)$(HOPBINDIR)/$(EXEC).jar

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	/bin/rm -f $(DESTDIR)$(HOPBINDIR)/$(EXEC)-$(HOPRELEASE)
	/bin/rm -f $(DESTDIR)$(HOPBINDIR)/$(EXEC)
	/bin/rm -f $(DESTDIR)$(HOPBINDIR)/$(EXEC).jar

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	/bin/rm -f .afile
	/bin/rm -f .jfile
	/bin/rm -f $(OBJECTS)
	/bin/rm -f $(MODULES:%=%.c)
	/bin/rm -rf o
	/bin/rm -f $(BUILDBINDIR)/$(EXEC)
	/bin/rm -f $(BUILDBINDIR)/$(EXEC).jar

devclean: clean

cleanall: devclean

#*---------------------------------------------------------------------*/
#*    Dependencies                                                     */
#*---------------------------------------------------------------------*/
$(OBJECTS): .afile

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
o/pipeline.o ./pipeline.class: stage.sch 
o/accept.o ./accept.class: stage.sch 
o/accept.o accept.class: stage.sch 
o/pipeline.o pipeline.class: stage.sch 
o/nothread_scheduler.o nothread_scheduler.class: stage.sch 
o/nothread_scheduler.o ./nothread_scheduler.class: stage.sch 

#bdepend stop
