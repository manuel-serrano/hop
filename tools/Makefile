#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/tools/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  serrano                                           */
#*    Creation    :  Fri Feb 18 17:48:08 2000                          */
#*    Last change :  Tue Mar 26 07:42:40 2024 (serrano)                */
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

build:  o
	$(MAKE) progs

progs: $(BUILDBINDIR)/testjs $(BUILDBINDIR)/hop2js $(BUILDBINDIR)/mkjsast

o: 
	mkdir -p $@


$(BUILDBINDIR)/testjs: .afile .etags o/testjs.o
	@ $(call link,$(BIGLOO),$(BCFLAGS),,o/testjs.o,-o,$@)

$(BUILDBINDIR)/hop2js: .afile .etags o/hop2js.o
	@ $(call link,$(BIGLOO),$(BCFLAGS),,o/hop2js.o,-o,$@)

$(BUILDBINDIR)/mkjsast: .afile .etags o/mkjsast.o
	@ $(call link,$(BIGLOO),$(BCFLAGS),,o/mkjsast.o,-o,$@)

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .o

#*---------------------------------------------------------------------*/
#*    The implicit `.scm.o' rule                                       */
#*---------------------------------------------------------------------*/
o/%.o: %.scm .afile
	@ $(call compile2,$(BIGLOO),$(BCFLAGS),$(BHFLAGS),-c,$< -o $@)

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags dep

.afile: $(SOURCES)
	@ $(AFILE) -o .afile $(SOURCES) 

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
	$(INSTALL) $(BUILDBINDIR)/hop2js $(DESTDIR)$(HOPBINDIR)
	chmod $(MODEXE) $(DESTDIR)$(HOPBINDIR)/hop2js
	$(INSTALL) $(BUILDBINDIR)/mkjsast $(DESTDIR)$(HOPBINDIR)
	chmod $(MODEXE) $(DESTDIR)$(HOPBINDIR)/mkjsast

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	/bin/rm -f $(DESTDIR)$(HOPBINDIR)/testjs
	/bin/rm -f $(DESTDIR)$(HOPBINDIR)/hop2js
	/bin/rm -f $(DESTDIR)$(HOPBINDIR)/mkjsast

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	/bin/rm -f .afile
	/bin/rm -f $(MODULES:%=%.c)
	/bin/rm -rf o
	/bin/rm -f $(BUILDBINDIR)/testjs
	/bin/rm -f $(BUILDBINDIR)/hop2js
	/bin/rm -f $(BUILDBINDIR)/mkjsast
	/bin/rm -rf META-INF

devclean: clean

distclean: devclean

cleanall: distclean

#*---------------------------------------------------------------------*/
#*    Dependencies                                                     */
#*---------------------------------------------------------------------*/
$(OBJECTS): .afile

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/

#bdepend stop
