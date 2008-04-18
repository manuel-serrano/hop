#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/weblets/wiki/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Fri Apr 18 11:45:57 2008 (serrano)                */
#*    Copyright   :  2006-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the wiki weblet                                  */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    CONFIG ...                                                       */
#*---------------------------------------------------------------------*/
TITLE = Wiki
VERSION = 1.1.0
MINHOP = $(HOPRELEASE) 
MAXHOP =
CATEGORY = hop
DATE = $(HOPDATE)

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION = Makefile wiki.hss wiki.hop config.hop.in \
             etc/weblet.info.in etc/wiki.wiki.in \
             etc/logo.png etc/favicon.png

build: etc/weblet.info etc/wiki.wiki config.hop .afile 

clean:

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version
include ../../etc/Makefile.weblet
