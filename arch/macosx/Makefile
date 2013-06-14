#*=====================================================================*/
#*    serrano/prgm/project/hop/2.4.x/arch/macosx/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Mar  2 07:14:10 2011                          */
#*    Last change :  Fri Jun 14 10:00:18 2013 (serrano)                */
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
  README Makefile.bundle Info.plist hop.icns macosx_bundle.h \
  HopIcon.rsrc \
  $(POPULATION_SRC:%=src/%) 

POPULATION_SRC=$(POPULATION_SRC_HOP:%=Hop/%)

POPULATION_SRC_HOP=ic_hop.icns ic_hop114.png \
  $(POPULATION_SRC_HOP_HOP) \
  $(POPULATION_SRC_HOP_XCODEPROJ:%=Hop.xcodeproj/%) \
  $(POPULATION_SRC_HOP_icons)

POPULATION_SRC_HOP_XCODEPROJ=project.pbxproj \
  $(POPULATION_SRC_HOP_XCODEPROJ_XCWORKSPACE:%=project.xcworkspace/%)

POPULATION_SRC_HOP_XCODEPROJ_XCWORKSPACE=contents.xcworkspacedata


POPDIRS = 

