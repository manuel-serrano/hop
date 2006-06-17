#*=====================================================================*/
#*    serrano/prgm/project/hop/src/Makefile                            */
#*    -------------------------------------------------------------    */
#*    Author      :  serrano                                           */
#*    Creation    :  Fri Feb 18 17:48:08 2000                          */
#*    Last change :  Sat Jun 17 15:26:52 2006 (serrano)                */
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
jvm: $(CLASS_DIR) $(BUILDBINDIR)/$(EXEC).jvm

o: 
	mkdir -p $@

$(CLASS_DIR):
	mkdir -p $@

$(BUILDBINDIR)/$(EXEC): .afile .etags $(OBJECTS)
	@ $(call compile,BIGLOO,BCFLAGS,-o $@ $(OBJECTS))
	@ $(call done,$(COLOR),"Binary ",$@,"...done.")

$(BUILDBINDIR)/$(EXEC).jvm: .afile .etags .jfile $(BGL_CLASSES)
	@ $(call compile,BIGLOO,BJVMFLAGS,-o $@ -I o/class_s $(BGL_CLASSES:o/class_s/%=%))
	@ $(call done,$(COLOR),"Binary ",$@,"...done.")

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .o

#*---------------------------------------------------------------------*/
#*    The implicit `.scm.o' rule                                       */
#*---------------------------------------------------------------------*/
o/%.o: %.scm
	@ $(call compile,BIGLOO,BCFLAGS,$< -o $@ -c)

$(CLASS_DIR)/%.class: %.scm
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
install:
	mkdir -p $(HOPBINDIR)
	install -m $(BMASK) $(BUILDBINDIR)/$(EXEC) $(HOPBINDIR)

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
	/bin/rm -f $(BGL_CLASSES)
	/bin/rm -f $(BUILDBINDIR)/$(EXEC)

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
