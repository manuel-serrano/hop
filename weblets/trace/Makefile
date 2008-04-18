#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/weblets/trace/Makefile            */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Fri Apr 18 11:49:52 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the trace weblet                                 */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = trace
VERSION = 1.0.0
MINHOP = $(HOPRELEASE) 
MAXHOP =
CATEGORY = hop
DATE = $(HOPDATE)

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION = Makefile trace.hop \
             etc/weblet.info.in etc/trace.wiki.in \
             etc/logo.png etc/favicon.png

build: etc/weblet.info etc/trace.wiki .afile

clean:

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version
include ../../etc/Makefile.weblet

