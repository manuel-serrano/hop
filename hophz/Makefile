#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/hophz/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  serrano                                           */
#*    Creation    :  Fri Feb 18 17:48:08 2000                          */
#*    Last change :  Tue Mar  4 07:57:58 2025 (serrano)                */
#*    -------------------------------------------------------------    */
#*    The Makefile that builds Hophz                                   */
#*=====================================================================*/

do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version

BLFLAGS		= 
BLINKFLAGS 	= -suffix hop $(BLDFLAGS)

#*---------------------------------------------------------------------*/
#*    Target and Project                                               */
#*---------------------------------------------------------------------*/
EXEC 		= hophz

#*---------------------------------------------------------------------*/
#*    Sources    					               */
#*---------------------------------------------------------------------*/
SCMMODULES 	= hophz_param parseargs main login action
HOPMODULES 	= api weblet config

MODULES 	= $(SCMMODULES) $(HOPMODULES)

INCLUDES 	= 

OBJECTS 	= $(MODULES:%=o/%.o)

SOURCES 	= $(SCMMODULES:%=%.scm) $(HOPMODULES:%=../weblets/hz/%.hop)

COLOR 		= 32

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION 	= $(INCLUDES) $(SOURCES) Makefile

#*---------------------------------------------------------------------*/
#*    The roost executable                                             */
#*---------------------------------------------------------------------*/
.PHONY: build dobuild

build: o $(BACKEND)

native: o $(BUILDBINDIR)/$(EXEC)

o: 
	mkdir -p $@

$(CLASS_DIR):
	mkdir -p $@

$(BUILDBINDIR)/$(EXEC): .afile $(OBJECTS)
	@ $(call link,$(BIGLOO),$(BCFLAGS),$(BLDFLAGS),$(OBJECTS),-o,$@)

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .o

#*---------------------------------------------------------------------*/
#*    The implicit `.scm.o' rule                                       */
#*---------------------------------------------------------------------*/
o/%.o: %.scm .afile
	@ $(call compile,$(BIGLOO),$(BCFLAGS),-c,$< -o $@)

o/%.o: o/%.scm
	@ $(call compile2,$(BIGLOO),$(BCFLAGS),-srfi hopc, -c,$< -o $@)

o/%.scm: ../weblets/hz/%.hop
	@ $(call compile3,$(HOPC),$(HFLAGS),$(BCFLAGS),$(BLFLAGS),-s,$< -o $@)

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile dep

.afile: $(SOURCES)
	@ $(AFILE) -o .afile $(SOURCES) 

.etags: .afile
	@ $(BTAGS) -o .etags $(SOURCES)

getsources:
	@ echo $(SOURCES)

#*---------------------------------------------------------------------*/
#*    predistrib                                                       */
#*---------------------------------------------------------------------*/
.PHONY: predistrib predistrib-src predistrib-clean

predistrib: $(HOPMODULES:%=o/%.scm)

predistrib-clean:
	$(RM) $(HOPMODULES:%=o/%.scm)

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
	$(INSTALL) $(BUILDBINDIR)/$(EXEC) $(DESTDIR)$(HOPBINDIR)
	chmod $(MODEXE) $(DESTDIR)$(HOPBINDIR)/$(EXEC)

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	$(RM) -f $(DESTDIR)$(HOPBINDIR)/$(EXEC)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	$(RM) -f .afile
	$(RM) -f $(OBJECTS)
	$(RM) -f $(MODULES:%=%.c)
	$(RM) -f $(HOPMODULES:%=o/%.scm)
	$(RM) -f $(BUILDBINDIR)/$(EXEC)

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

#bdepend stop
