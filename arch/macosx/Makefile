#*=====================================================================*/
#*    serrano/prgm/project/hop/macosx/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Mar  2 07:14:10 2011                          */
#*    Last change :  Tue Sep 10 08:17:11 2013 (serrano)                */
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
  $(COMMON_POPULATION:%=common/%) \
  $(GUI_POPULATION:%=gui/%) \
  $(NOGUI_POPULATION:%=nogui/%) \
  README Makefile.bundle \
  $(POPULATION_SRC:%=src/%) 

COMMON_POPULATION = hop.icns HopIcon.rsrc
NOGUI_POPULATION = Info.plist

POPULATION_GUI=$(POPULATION_GUI_HOP:%=Hop/%)

POPULATION_GUI_HOP=ic_hop.icns ic_hop114.png \
  $(POPULATION_GUI_HOP_HOP:%=Hop/%) \
  $(POPULATION_GUI_HOP_XCODEPROJ:%=Hop.xcodeproj/%) \
  $(POPULATION_GUI_HOP_ICONS:%=icons/%)

POPULATION_GUI_HOP_XCODEPROJ=project.pbxproj \
  $(POPULATION_GUI_HOP_XCODEPROJ_XCWORKSPACE:%=project.xcworkspace/%)

POPULATION_GUI_HOP_XCODEPROJ_XCWORKSPACE=contents.xcworkspacedata

POPULATION_GUI_HOP_ICONS=ic_green_light.png ic_menu_goto.png \
  ic_menu_refresh.png ic_menu_block.png ic_menu_info_details.png \
  ic_menu_stop.png ic_menu_clear_playlist.png ic_menu_preferences.png \
  ic_red_light.png

POPULATION_GUI_HOP_HOP=Hop-Info.plist Hop10.6-Info.plist Hop-Prefix.pch \
 Hop10.7-Info.plist Hop10.8-Info.plist Hop.h HopAppDelegate.h \
 Hop.entitlements Hop.m HopAppDelegate.m main.m \
 $(POPULATION_GUI_HOP_HOP_EN:%=en.lproj/%)

POPULATION_GUI_HOP_HOP_EN=Credits.rtf InfoPlist.strings MainMenu.xib

POPDIRS = 

