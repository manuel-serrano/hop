#*=====================================================================*/
#*    serrano/prgm/project/hop/3.0.x/src/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  serrano                                           */
#*    Creation    :  Fri Feb 18 17:48:08 2000                          */
#*    Last change :  Wed Feb 26 17:44:22 2014 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The Makefile that builds Hop                                     */
#*=====================================================================*/
do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version

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

link.dynamic:
	@ $(call link,$(BIGLOO),$(BCFLAGS) $(BLDFLAGS),$(BCFLAGSDEV),$(OBJECTS),-o,$(DEST))

link.static:
	@ $(call link,$(BIGLOO),$(BCFLAGS),$(BCFLAGSDEV),-static-all-bigloo $(OBJECTS),-o,$(DEST))

link.library:
	echo "***ERROR: link.library not currently supported!"
	exit 1

$(BUILDBINDIR)/$(EXEC).jar: .afile .etags .jfile $(BGL_CLASSES) jvm-stdlibs jvm-share jvm-etc jvm-lib META-INF/MANIFEST.MF
	@ $(MAKE) jar.$(LINK) DEST=$@
	@ $(call done,$@)

jar.dynamic: .afile .etags .jfile $(BGL_CLASSES) jvm-stdlibs jvm-share jvm-etc jvm-lib
	$(JAR) $(DEST) META-INF/MANIFEST.MF -C o/class_s .

jar.felix: .afile .etags .jfile $(BGL_CLASSES) jvm-stdlibs jvm-share jvm-etc jvm-lib 
	@ $(MAKE) -C $(BUILDDIR)/arch/felix
	$(JAR) $(DEST) META-INF/MANIFEST.MF -C o/class_s .

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
	$(RM) -rf o/class_s/share
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

META-INF/MANIFEST.MF: META-INF
	@ $(MAKE) manifest.$(LINK) DEST=$@

manifest.dynamic:
	@ echo "Manifest-Version: 1.0" > $(DEST)
	@ echo "Main-Class: bigloo/hop/main" >> $(DEST)
	@ echo "Created-By: Hop $(VERSION)" >> $(DEST)

manifest.felix:
	@ $(MAKE) manifest.dynamic
	@ echo "Bundle-Name: Hop controller" >> $(DEST)
	@ echo "Bundle-Description: Manages a Hop instance." >> $(DEST)
	@ echo "Bundle-Vendor: INRIA" >> $(DEST)
	@ echo "Bundle-Version: 0.1" >> $(DEST)
	@ echo "Bundle-Activator: bigloo.hop.HopActivator" >> $(DEST)
	@ echo "Import-Package: org.osgi.framework" >> $(DEST)

META-INF:
	mkdir -p $@

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .o

#*---------------------------------------------------------------------*/
#*    Implicit rules                                                   */
#*---------------------------------------------------------------------*/
o/%.o: %.scm .afile
	@ $(call compile,$(BIGLOO),$(BCFLAGS),-c,$< -o $@)

$(CLASS_DIR)/%.class: %.scm .afile
	@ $(call compile,$(BIGLOO),$(BJVMFLAGS),-c,$< -o $@)

o/main.o: main.scm
	@ $(call compile2,$(BIGLOO),$(BCFLAGS),$(BCFLAGSDEV),-c,$< -o $@)

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
	(cd $(DESTDIR)$(HOPBINDIR) && $(RM) -f hop && ln hop-$(HOPRELEASE) hop)

install-jvm:
	$(INSTALL) $(BUILDBINDIR)/$(EXEC).jar $(DESTDIR)$(HOPBINDIR)
	chmod $(MODFILE) $(DESTDIR)$(HOPBINDIR)/$(EXEC).jar

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	$(RM) -f $(DESTDIR)$(HOPBINDIR)/$(EXEC)-$(HOPRELEASE)
	$(RM) -f $(DESTDIR)$(HOPBINDIR)/$(EXEC)
	$(RM) -f $(DESTDIR)$(HOPBINDIR)/$(EXEC).jar

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	$(RM) -f .afile
	$(RM) -f .jfile
	$(RM) -f $(OBJECTS)
	$(RM) -f $(MODULES:%=%.c)
	$(RM) -rf o
	$(RM) -f $(BUILDBINDIR)/$(EXEC)
	$(RM) -f $(BUILDBINDIR)/$(EXEC).jar
	$(RM) -f META-INF/MANIFEST.MF

devclean: clean

distclean: clean

cleanall: devclean

#*---------------------------------------------------------------------*/
#*    Dependencies                                                     */
#*---------------------------------------------------------------------*/
$(OBJECTS): .afile

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
o/pipeline.o pipeline.class: stage.sch 
o/init.o ./init.class: libraries.sch 
o/pipeline.o ./pipeline.class: stage.sch 
o/main.o main.class: libraries.sch 
o/nothread_scheduler.o ./nothread_scheduler.class: stage.sch 
o/accept.o ./accept.class: stage.sch 
o/init.o init.class: libraries.sch 
o/nothread_scheduler.o nothread_scheduler.class: stage.sch 
o/accept.o accept.class: stage.sch 

#bdepend stop
