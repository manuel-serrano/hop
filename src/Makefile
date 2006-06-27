#*=====================================================================*/
#*    serrano/prgm/project/hop/src/Makefile                            */
#*    -------------------------------------------------------------    */
#*    Author      :  serrano                                           */
#*    Creation    :  Fri Feb 18 17:48:08 2000                          */
#*    Last change :  Thu Jun 22 08:26:09 2006 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The Makefile that builds Hop                                     */
#*=====================================================================*/

do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../etc/Makefile.hopconfig
include ../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Target and Project                                               */
#*---------------------------------------------------------------------*/
EXEC		= hop

#*---------------------------------------------------------------------*/
#*    Sources    					               */
#*---------------------------------------------------------------------*/
MODULES		= hop-param parseargs main init

INCLUDES	= eval-macro.sch compiler-macro.sch

CLASS_DIR	= o/class_s/bigloo/hop

OBJECTS 	= $(MODULES:%=o/%.o)
BGL_CLASSES	= $(MODULES:%=$(CLASS_DIR)/%.class)

SOURCES 	= $(MODULES:%=%.scm)

COLOR		= 32

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION 	= $(INCLUDES) Makefile

#*---------------------------------------------------------------------*/
#*    The roost executable                                             */
#*---------------------------------------------------------------------*/
.PHONY: build dobuild

build:  o $(BACKEND)


native: $(BUILDBINDIR)/$(EXEC)
jvm: $(CLASS_DIR) $(BUILDBINDIR)/$(EXEC).jar

o: 
	mkdir -p $@

$(CLASS_DIR):
	mkdir -p $@

$(BUILDBINDIR)/$(EXEC): .afile .etags $(OBJECTS)
	@ $(call compile,BIGLOO,BCFLAGS,-o $@ $(OBJECTS))
	@ $(call done,$(COLOR),"Binary ",$@,"...done.")

$(BUILDBINDIR)/$(EXEC).jar: .afile .etags .jfile $(BGL_CLASSES) META-INF/MANIFEST.MF jvm-stdlibs jvm-share jvm-lib
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
	cp -r ../share o/class_s
	cp -r ../scheme2js/runtime/*.js o/class_s/share
	cp -r ../weblets o/class_s/share
	$(MAKE) -s jvm-list SRC=.. DIR=weblets DEST=o/class_s/share

jvm-list:
	/usr/bin/ls $(SRC)/$(DIR) > $(DEST)/$(DIR)/.list
	for p in `ls $(SRC)/$(DIR)`; do \
	  if [ -d $(SRC)/$(DIR)/$$p ]; then \
            $(MAKE) jvm-list SRC=$(SRC)/$(DIR) DIR=$$p DEST=$(DEST)/$(DIR) || (echo "*** ERROR: $$p"; exit 1); \
          fi \
        done

jvm-lib: o/class_s/lib/hop/$(HOPRELEASE)
	cp $(BUILDLIBDIR)/*.init $<

META-INF/MANIFEST.MF: META-INF
	echo "Manifest-Version: 1.0" > $@
	echo "Main-Class: bigloo/hop/main" >> $@
	echo "Created-By: Hop $(VERSION)" >> $@

META-INF:
	mkdir -p $@

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
          head -n `expr $$num - 1` Makefile > /tmp/Makefile.aux)
	@ $(DEPEND) -strict-obj-dir o -fno-mco $(SOURCES) >> /tmp/Makefile.aux
	@ mv /tmp/Makefile.aux Makefile

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: install-$(BACKEND)

install-native:
	mkdir -p $(HOPBINDIR)
	install -m $(BMASK) $(BUILDBINDIR)/$(EXEC) $(HOPBINDIR)

install-jvm:
	mkdir -p $(HOPBINDIR)
	install -m $(BMASK) $(BUILDBINDIR)/$(EXEC).jar $(HOPBINDIR)

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	/bin/rm -f $(HOPBINDIR)/$(EXEC)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	/bin/rm -f .afile
	/bin/rm -f $(OBJECTS)
	/bin/rm -f $(MODULES:%=%.c)
	/bin/rm -rf o
	/bin/rm -f $(BUILDBINDIR)/$(EXEC)
	/bin/rm -f $(BUILDBINDIR)/$(EXEC).jar
	/bin/rm -rf META-INF

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

#bdepend stop
