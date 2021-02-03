#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/arch/android/hzdemo/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  manuel serrano                                    */
#*    Creation    :  Fri Nov 13 15:01:33 2020                          */
#*    Last change :  Fri Nov 13 15:01:33 2020 (serrano)                */
#*    Copyright   :  2020 manuel serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the hzdemo                                 */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration (grabbed from package.json).                       */
#*---------------------------------------------------------------------*/
HOP = hop --no-server --evaljs
HOPDIR = $(shell realpath $$PWD/../../..)

HZ = $(shell $(HOP) "console.log( require( './package.json' ).name )")
HZVERSION = $(shell $(HOP) "console.log( require( './package.json' ).version )")


#*---------------------------------------------------------------------*/
#*    apk                                                              */
#*---------------------------------------------------------------------*/
apk: /tmp/$(HZ)-$(HZVERSION).hz $(HZ)-$(HZVERSION).apk
	@echo "to install:"
	@echo "  adb install -r /tmp/build.$(HZ)-$(HZVERSION).android/$(HZ)-$(HZVERSION).apk"
	@echo "  adb shell monkey -p fr.inria.$(HZ) 1"

$(HZ)-$(HZVERSION).apk: \
  arch/android/Makefile.config \
  /tmp/$(HZ)-$(HZVERSION).hz
	make -C arch/android -f Makefile.android \
           DESTDIR=$$PWD \
           HOPHZ=/tmp/$(HZ)-$(HZVERSION).hz

#*---------------------------------------------------------------------*/
#*    Makefile.config                                                  */
#*---------------------------------------------------------------------*/
arch/android/Makefile.config: arch/android/Makefile.config.in
	cat $< \
	    | sed -e "s|@HOPAPP@|$(HZ)|g" \
	          -e "s|@HOPAPKVERSION@|$(HZVERSION)|g" \
	          -e "s|@ANDROIDWEBLETDIR@|$$PWD|g" \
	          -e "s|@HOPDIR@|$(HOPDIR)|g" \
	    >> $@

#*---------------------------------------------------------------------*/
#*    hz                                                               */
#*---------------------------------------------------------------------*/
/tmp/$(HZ)-$(HZVERSION).hz: package.json $(HZ).js
	(cd ..; \
	 cp -r $(HZ) $(HZ)-$(HZVERSION); \
	 tar cvfz $@ \
           --exclude='$(HZ)/private' \
           --exclude='*.apk' \
           --exclude=.gitignore \
           --exclude=.git \
           --exclude=arch/debian/build.$(HZ) \
           --exclude='*~' \
           $(HZ)-$(HZVERSION); \
         rm -rf $(HZ)-$(HZVERSION))

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	rm -f /tmp/$(HZ)-$(HZVERSION).hz
	rm -f arch/android/Makefile.config
