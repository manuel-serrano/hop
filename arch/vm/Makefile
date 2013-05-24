#*=====================================================================*/
#*    serrano/prgm/project/hop/2.4.x/arch/vm/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Mar  2 07:14:10 2011                          */
#*    Last change :  Wed May 22 20:10:23 2013 (serrano)                */
#*    Copyright   :  2011-13 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    VirtualMachine image generation                                  */
#*=====================================================================*/

do: usage

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../../etc/Makefile.hopconfig
-include ../../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Default settings                                                 */
#*---------------------------------------------------------------------*/
IMGSIZE = 4G
WEBLETS = hoptex-1.4.6.hz popart-api-1.0.0.hz hopslide-2.5.7.hz \
  hopimpress-0.3.0.hz flipcounter-api-1.0.0.hz qrcode-api-1.1.0.hz
EXTRAWEBLETS = 

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION = README Makefile \
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
.PHONY: qemu qemu+ virtualbox virtualbox+ vmware vmware+ do

usage:
	@ echo "usage: make virtualbox+ HOPTMPDIR=/misc/virtual/tmp RELEASE=4.0b-alpha21May13 HOPRELEASE=2.4.1"

$(HOPTMPDIR)/hop.hdd:
	sudo scripts/create_image.sh $(HOPTMPDIR)/hop.hdd $(IMGSIZE)
	sudo chown $(USER) $(HOPTMPDIR)/hop.hdd
	sudo scripts/install_bigloo_hop.sh $(HOPTMPDIR)/hop.hdd $(RELEASE) $(HOPRELEASE)

$(HOPTMPDIR)/hop+.hdd:
	sudo scripts/create_image.sh $(HOPTMPDIR)/hop+.hdd $(IMGSIZE)
	sudo chown $(USER) $(HOPTMPDIR)/hop+.hdd
	sudo scripts/install_bigloo_hop.sh $(HOPTMPDIR)/hop+.hdd $(RELEASE) $(HOPRELEASE) $(WEBLETS:%=$(REPODIR)/%) $(EXTRAWEBLETS)

$(HOPTMPDIR)/hop.vmdk: $(HOPTMPDIR)/hop.hdd
	scripts/convert_image.sh $(HOPTMPDIR)/hop

$(HOPTMPDIR)/hop+.vmdk: $(HOPTMPDIR)/hop+.hdd
	scripts/convert_image.sh $(HOPTMPDIR)/hop+

qemu: $(HOPTMPDIR)/hop.hdd
qemu+: $(HOPTMPDIR)/hop+.hdd

virtualbox: $(HOPTMPDIR)/hop.vmdk
virtualbox+: $(HOPTMPDIR)/hop+.vmdk

