#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/http/Makefile                       */
#*    -------------------------------------------------------------    */
#*    Author      :  Florian Loitsch                                   */
#*    Creation    :  Wed Mar 15 07:27:50 2006                          */
#*    Last change :  Mon Nov  4 11:55:31 2024 (serrano)                */
#*    Copyright   :  2006-24 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the hopsched library.                     */
#*=====================================================================*/
.PHONY: do install

do: build
install: install-library

#*---------------------------------------------------------------------*/
#*    Standard Bigloo configuration                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version
-include $(BIGLOOLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the library name
TARGETNAME = http

BLIBFLAGS = $(BSCM2JSFLAGS)

#*---------------------------------------------------------------------*/
#*    Scheme extended objects                                          */
#*---------------------------------------------------------------------*/
_BGL_OBJECTS = types utils parser accept pipeline scheduler \
  amany_scheduler nothread_scheduler oto_scheduler \
  pool_scheduler queue_scheduler 

_OBJECTS = $(_BGL_OBJECTS)

OBJECTS = $(_OBJECTS:%=o/%.o)
EOBJECTS = o/make_lib.o

BGL_CLASSES = $(_OBJECTS:%=$(CLASS_DIR)/%.class)
BGL_ECLASSES = $(CLASS_EDIR)/make_lib.class

SOURCES = $(_OBJECTS:%=%.scm)

INCLUDES =

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES) $(INCLUDES) make_lib.scm Makefile

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
include ../etc/Makefile.library

.PHONY: build native jvm

build: .afile
	$(MAKE) $(BACKEND)

native: o heap-c lib-c
jvm: heap-jvm lib-jvm

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags .jfile
	@ $(MAKE) dep

.afile: $(SOURCES)
	@ $(AFILE) -o .afile $(SOURCES)

.jfile: $(SOURCES)
	@ $(JFILE) -o .jfile \
                   -pbase $(PBASE) $(SOURCES) make_lib.scm

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(SOURCES)

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
o/%.o: .afile

#bdepend stop
