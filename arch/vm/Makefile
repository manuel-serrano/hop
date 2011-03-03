#*=====================================================================*/
#*    serrano/prgm/project/hop/2.2.x/arch/vm/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Mar  2 07:14:10 2011                          */
#*    Last change :  Wed Mar  2 14:27:25 2011 (serrano)                */
#*    Copyright   :  2011 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    VirtualMachine image generation                                  */
#*=====================================================================*/

do: qemu

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../etc/Makefile.hopconfig
-include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Default settings                                                 */
#*---------------------------------------------------------------------*/
IMGSIZE = 4G

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION = Makefile \
  README.wiki \
  scripts/compile_bigloo_hop.sh scripts/create_image.sh \
  scripts/convert_image.sh scripts/install_bigloo_hop.sh

POPDIRS = 

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
$(HOPTMPDIR)/hop.hdd:
	sudo scripts/create_image.sh $(HOPTMPDIR)/hop.hdd $(IMGSIZE)

hop-install: $(HOPTMPDIR)/hop.hdd
	sudo scripts/install_bigloo_hop.sh $(HOPTMPDIR)/hop.hdd $(RELEASE) $(HOPRELEASE)

qemu: hop-install

