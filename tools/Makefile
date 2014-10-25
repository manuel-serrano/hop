#*=====================================================================*/
#*    serrano/prgm/project/hop/3.0.x/tools/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  serrano                                           */
#*    Creation    :  Fri Feb 18 17:48:08 2000                          */
#*    Last change :  Thu Sep 25 10:05:12 2014 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The Makefile that builds Hopc                                    */
#*=====================================================================*/

do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Sources    					               */
#*---------------------------------------------------------------------*/
MODULES		= testjs

INCLUDES	= 

SOURCES 	= $(MODULES:%=%.scm)

COLOR		= 32

BHFLAGS		= -srfi hopc

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION 	= $(INCLUDES) $(SOURCES) Makefile

#*---------------------------------------------------------------------*/
#*    The roost executable                                             */
#*---------------------------------------------------------------------*/
.PHONY: build dobuild

build:  $(BUILDBINDIR)/testjs # $(BUILDBINDIR)/heaptojs

o: 
	mkdir -p $@


$(BUILDBINDIR)/testjs: .afile .etags o/testjs.o
	@ $(call link,$(BIGLOO),$(BCFLAGS),,o/testjs.o,-o,$@)

$(BUILDBINDIR)/heaptojs: .afile .etags o/heaptojs.o
	@ $(call link,$(BIGLOO),$(BCFLAGS),,o/heaptojs.o,-o,$@)

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .o

#*---------------------------------------------------------------------*/
#*    The implicit `.scm.o' rule                                       */
#*---------------------------------------------------------------------*/
o/%.o: %.scm .afile o
	@ $(call compile2,$(BIGLOO),$(BCFLAGS),$(BHFLAGS),-c,$< -o $@)

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
	$(INSTALL) $(BUILDBINDIR)/testjs $(DESTDIR)$(HOPBINDIR)
	chmod $(MODEXE) $(DESTDIR)$(HOPBINDIR)/testjs
	$(INSTALL) $(BUILDBINDIR)/heaptojs $(DESTDIR)$(HOPBINDIR)
	chmod $(MODEXE) $(DESTDIR)$(HOPBINDIR)/heaptojs

install-jvm:
	$(INSTALL) $(BUILDBINDIR)/testjs.jar $(DESTDIR)$(HOPBINDIR)
	chmod $(MODFILE) $(DESTDIR)$(HOPBINDIR)/testjs.jar
	$(INSTALL) $(BUILDBINDIR)/heaptojs.jar $(DESTDIR)$(HOPBINDIR)
	chmod $(MODFILE) $(DESTDIR)$(HOPBINDIR)/heaptojs.jar

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	/bin/rm -f $(DESTDIR)$(HOPBINDIR)/testjs
	/bin/rm -f $(DESTDIR)$(HOPBINDIR)/heaptojs

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	/bin/rm -f .afile
	/bin/rm -f $(MODULES:%=%.c)
	/bin/rm -rf o
	/bin/rm -f $(BUILDBINDIR)/testjs
	/bin/rm -f $(BUILDBINDIR)/heaptojs
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
