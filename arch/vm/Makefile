#*=====================================================================*/
#*    serrano/prgm/project/hop/2.2.x/arch/vm/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Mar  2 07:14:10 2011                          */
#*    Last change :  Mon Mar  7 10:41:15 2011 (serrano)                */
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
WEBLETS = hoptex-1.1.2.hz popart-api-1.0.0.hz hopslide-2.5.1.hz

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION = Makefile \
  README.wiki \
  scripts/compile_bigloo_hop.sh scripts/create_image.sh \
  scripts/convert_image.sh scripts/install_bigloo_hop.sh script/mount_hdd.sh

POPDIRS = 

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
.PHONY: qemu qemuw virtualbox virtualbox+ vmware vmware+

$(HOPTMPDIR)/hop.hdd:
	sudo scripts/create_image.sh $(HOPTMPDIR)/hop.hdd $(IMGSIZE)
	sudo chown $(USER) $(HOPTMPDIR)/hop.hdd

hop-install: $(HOPTMPDIR)/hop.hdd
	sudo scripts/install_bigloo_hop.sh $(HOPTMPDIR)/hop.hdd $(RELEASE) $(HOPRELEASE)

hop-install+: $(HOPTMPDIR)/hop.hdd
	mv $(HOPTMPDIR)/hop.hdd $(HOPTMPDIR)/hop+.hdd
	sudo scripts/install_bigloo_hop.sh $(HOPTMPDIR)/hop+.hdd $(RELEASE) $(HOPRELEASE) $(WEBLETS:%=$(REPOSITORY/%))

$(HOPTMPDIR)/hop.vmdk: $(HOPTMPDIR)/hop.hdd
	scripts/convert_image.sh $(HOPTMPDIR)/hop

$(HOPTMPDIR)/hop+.vmdk: $(HOPTMPDIR)/hop+.hdd
	scripts/convert_image.sh $(HOPTMPDIR)/hop+

qemu: hop-install
qemu+: hop-install+

virtualbox: $(HOPTMPDIR)/hop.vmdk
virtualbox+: $(HOPTMPDIR)/hop+.vmdk

