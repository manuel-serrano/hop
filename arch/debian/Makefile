#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/arch/debian/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri May 13 10:19:41 2011                          */
#*    Last change :  Mon Mar 29 07:11:44 2021 (serrano)                */
#*    Copyright   :  2011-21 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Debian (maemo) port                                              */
#*=====================================================================*/

do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../etc/Makefile.hopconfig
-include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
MAEMO = Makefile.maemo \
  hop-16x16.png hop-26x26.png hop-40x40.png hop-48x48.png hop-64x64.png \
  hop.desktop.in hop.install hop.service.in

LAUNCHER = Makefile.in configure.in hop-128x128.png hop.c list.h param.h \
   user.c README gui.c hop-launcher.hop hop.h main.c read.c user.h \
   config.h.in gui.h hop-launcher.rc.in list.c param.c read.h

DEBIAN = changelog.in control.in postinst.in postrm.in rules.in

POPULATION = Makefile README \
  init.d/hop.in \
  makedeb.sh.in \
  $(MAEMO:%=maemo/%) \
  $(LAUNCHER:%=maemo/hop-launcher/%) \
  $(DEBIAN:%=debian/%)

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
build:
	makedeb.sh
