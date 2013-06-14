#*=====================================================================*/
#*    serrano/prgm/project/hop/2.4.x/arch/macosx/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Mar  2 07:14:10 2011                          */
#*    Last change :  Fri Jun 14 11:08:59 2013 (serrano)                */
#*    Copyright   :  2011-13 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    MacOS X port                                                     */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../etc/Makefile.hopconfig
-include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION = Makefile \
  README Makefile.bundle hop.icns macosx_bundle.h \
  HopIcon.rsrc \
  $(POPULATION_SRC:%=src/%) 

POPULATION_SRC=$(POPULATION_SRC_HOP:%=Hop/%)

POPULATION_SRC_HOP=ic_hop.icns ic_hop114.png \
  $(POPULATION_SRC_HOP_HOP:%=Hop/%) \
  $(POPULATION_SRC_HOP_XCODEPROJ:%=Hop.xcodeproj/%) \
  $(POPULATION_SRC_HOP_ICONS:%=icons/%)

POPULATION_SRC_HOP_XCODEPROJ=project.pbxproj \
  $(POPULATION_SRC_HOP_XCODEPROJ_XCWORKSPACE:%=project.xcworkspace/%)

POPULATION_SRC_HOP_XCODEPROJ_XCWORKSPACE=contents.xcworkspacedata

POPULATION_SRC_HOP_ICONS=ic_green_light.png ic_menu_goto.png \
  ic_menu_refresh.png ic_menu_block.png ic_menu_info_details.png \
  ic_menu_stop.png ic_menu_clear_playlist.png ic_menu_preferences.png \
  ic_red_light.png

POPULATION_SRC_HOP_HOP=Hop-Info.plist Hop-Prefix.pch Hop.h HopAppDelegate.h \
 Hop-Info.plist.orig Hop.entitlements Hop.m HopAppDelegate.m main.m \
 $(POPULATION_SRC_HOP_HOP_EN:%=en.lproj/%)

POPULATION_SRC_HOP_HOP_EN=Credits.rtf InfoPlist.strings MainMenu.xib

POPDIRS = 

