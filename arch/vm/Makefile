#*=====================================================================*/
#*    serrano/prgm/project/hop/2.4.x/arch/vm/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Mar  2 07:14:10 2011                          */
#*    Last change :  Wed Jun  6 11:16:56 2012 (serrano)                */
#*    Copyright   :  2011-12 Manuel Serrano                            */
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
WEBLETS = hoptex-1.2.0.hz popart-api-1.0.0.hz hopslide-2.5.4.hz

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION = Makefile \
  scripts/compile_bigloo_hop.sh scripts/create_image.sh \
  scripts/convert_image.sh scripts/install_bigloo_hop.sh scripts/mount_hdd.sh \
  config/boot/syslinux/extlinux.conf config/etc/apt/sources.list \
  config/etc/default/hop config/etc/hostname config/etc/init.d/hop \
  config/etc/network/interfaces config/etc/samba/smb.conf \
  config/usr/local/sbin/hop

POPDIRS = 

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
.PHONY: qemu qemu+ virtualbox virtualbox+ vmware vmware+

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

$(HOPTMPDIR)/hop+.vmdk: $(HOPTMPDIR)/hop.hdd
	scripts/convert_image.sh $(HOPTMPDIR)/hop+

qemu: hop-install
qemu+: hop-install+

virtualbox: $(HOPTMPDIR)/hop.vmdk
virtualbox+: $(HOPTMPDIR)/hop+.vmdk

