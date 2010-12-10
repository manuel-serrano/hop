#*=====================================================================*/
#*    serrano/prgm/project/hop/2.2.x/share/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 25 07:19:48 2004                          */
#*    Last change :  Wed Dec  8 11:15:21 2010 (serrano)                */
#*    Copyright   :  2004-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile for the HOP shared directory                        */
#*=====================================================================*/
.PHONY: do 

do: build

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    FLASHOBJS                                                        */
#*---------------------------------------------------------------------*/
FLASHOBJS	= HopServevt.swf HopAudio.swf HopVideo.swf \
                  JavaScriptFlashGateway.swf JavaScriptFlashGateway.js
FLASHSRC	= HopServevt.as HopAudio.as HopVideo.as
FLASH		= $(FLASHOBJS) $(FLASHSRC)
FLASHOPT	= player.swf swfobject.js

#*---------------------------------------------------------------------*/
#*    Runtime                                                          */
#*---------------------------------------------------------------------*/
RTS		= $(RTSCONFIG)

#*---------------------------------------------------------------------*/
#*    Client-side runtime library                                      */
#*    -------------------------------------------------------------    */
#*    See hopscheme/hop-runtime.scm to add new modules.                */
#*---------------------------------------------------------------------*/
EXTRA		= hop.hss \
                  hop-fx.js hop-fx.hss \
                  hop-paned.hss hop-paned.js \
                  hop-sorttable.hss hop-sorttable.js \
                  md5.js sha1.js aes.js \
		  hop-slider.hss hop-slider.js \
                  hop-tabslider.hss hop-tabslider.js \
	 	  hop-notepad.hss hop-notepad.js \
                  hop-tree.js hop-tree.css \
		  hop-foldlist.js hop-foldlist.css \
                  hop-window.scm hop-window.js \
                  hop-window.hss hop-window-red.hss hop-window-orange.hss \
                  hop-editor.js hop-editor.css \
                  hop-file.js hop-file.scm hop-file.hss \
                  hop-fileselect.js hop-fileselect.css \
                  hop-dashboard.js hop-dashboard.scm \
                  hop-prefs.js hop-prefs.hss \
                  hop-audio.js hop-audio.scm hop-audio.hss \
                  hop-video.js hop-video.hss \
                  hop-login.js hop-login.hss \
		  hop-spinbutton.scm hop-spinbutton.hss \
		  hop-color.scm hop-color.hss \
                  hop-lframe.hss \
                  hop-canvas.scm hop-exception.scm hop-orientation.scm \
                  hop-spage.scm hop-spage.hss hop-spage-iphone.hss\
                  hop-gauge.scm hop-gauge.hss \
                  hop-font.scm \
                  hop-password.scm

#*---------------------------------------------------------------------*/
#*    AUDIO and ICONS                                                  */
#*---------------------------------------------------------------------*/
ICONS_AUDIO	= mute.png pause.png playlist.png prefs.png stop.png \
		  next.png play.png podcast.png prev.png unmute.png

ICONS_PANED	= hcursor.png vcursor.png

ICONS_SLIDER	= cursor.png

ICONS_WINDOW	= bottom-left.png bottom-middle.png bottom-right.png \
		  close.png icon.png maximize.png \
                  title-left.png title.png title-right.png \
                  title-left-hover.png title-hover.png title-right-hover.png

ICONS_WINDOW_RED= close.png iconify.png maximize.png 

ICONS_FX	= shadow-e.png shadow-se.png shadow-ne.png \
                  shadow-sw.png shadow-s.png

ICONS_TREE	= device.png folder-close.png joinbottom.png plus.png \
		  empty.png folder-open.png minus.png plusbottom.png \
		  file.png join.png minusbottom.png vline.png

ICONS_FOLDLIST	= triangle-down.png triangle-right.png

ICONS_LOGIN	= login.png

ICONS_FCHOOSER	= folder-close.png file.png home.png hdd.png desktop.png \
                  keyboard.png drag.png \
                  edit-add.png edit-remove.png

ICONS_COLOR	= transbg.png satval.png 

ICONS_ACTIONS   = cancel.png ok.png run.png open.png

ICONS		= important.png shadow-se.png shadow-sw.png \
		  busy-anim-16.gif busy-anim-32.gif logo-bg.png \
		  dashboard.png notfound.png source.png \
		  resize-grip.png stop.png \
		  error.png client-error.png shadow-e.png timeout.png \
		  error2.png shadow-ne.png warning.png privacy.png \
		  hop-16x16.png hop-48x48.png hop-32x32.png hop-128x128.png \
                  hop-api-48x48.png hop-api-16x16.png \
                  hop.png hop.svg \
                  shadow-s.png window-handle.png \
		  $(ICONS_AUDIO:%=hop-audio/%) \
                  $(ICONS_PANED:%=hop-paned/%) \
                  $(ICONS_SLIDER:%=hop-slider/%) \
                  $(ICONS_WINDOW:%=hop-window/%) \
                  $(ICONS_WINDOW_RED:%=hop-window-red/%) \
                  $(ICONS_FX:%=hop-fx/%) \
                  $(ICONS_TREE:%=hop-tree/%) \
                  $(ICONS_FOLDLIST:%=hop-foldlist/%) \
                  $(ICONS_LOGIN:%=hop-login/%) \
                  $(ICONS_FCHOOSER:%=hop-filechooser/%) \
                  $(ICONS_COLOR:%=hop-color/%) \
                  $(ICONS_ACTIONS:%=actions/%)

BUTTONS		= cc.png firefox.png hop2.png inria.png opera.png \
		  css.png freebsd.png html401.png inria2.png paypal.png \
		  debian.png gpl.png ie1.png konqueror.png safari.png \
		  donate.png hop.png ie2.png mozilla.png xhtml.png rss.png
 
#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= $(RTS) $(EXTRA) \
                  $(ICONS:%=icons/%) \
                  $(BUTTONS:%=buttons/%) \
                  editor \
		  $(FLASH:%=flash/%) \
                  Makefile

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
build: .afile hop.js $(FLASHSRC:%.as=flash/%.swf) $(BACKEND) hop-password.scm

#*---------------------------------------------------------------------*/
#*    hop-password.scm ...                                             */
#*---------------------------------------------------------------------*/
hop-password.scm: ../runtime/password.scm
	@ echo ";; Generated file, don't edit" > $@
	@ cat $^ | sed -e "s|;; @client ||" -e "s|#;@server|;;|" >> $@

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	$(INSTALL) $(BUILDSHAREDIR)/.afile $(DESTDIR)$(HOPSHAREDIR)/.afile && \
	chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/.afile && \
	$(INSTALL) $(BUILDSHAREDIR)/hop-runtime.sch $(DESTDIR)$(HOPSHAREDIR)/hop-runtime.sch && \
	chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/hop-runtime.sch && \
        for p in hop.js hop-autoconf.js $(RTS); do \
	  $(INSTALL) $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$$p && \
	  chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$$p && \
	  $(GZIP) $(BUILDSHAREDIR)/$$p -c > $(DESTDIR)$(HOPSHAREDIR)/$$p.gz 2> /dev/null && \
          chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$$p.gz; \
        done
	$(INSTALL) ../scheme2js/runtime/runtime.js $(DESTDIR)$(HOPSHAREDIR)/runtime.js && \
	chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/runtime.js && \
	$(INSTALL) ../scheme2js/runtime/runtime.js $(DESTDIR)$(HOPSHAREDIR)/runtime.js && \
	$(GZIP) ../scheme2js/runtime/runtime.js -c > $(DESTDIR)$(HOPSHAREDIR)/runtime.js.gz && \
        chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/runtime.js.gz; \
        for p in $(EXTRA); do \
	  $(INSTALL) $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$$p && \
	  chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$$p; \
        done
	if [ ! -d $(DESTDIR)$(HOPSHAREDIR)/icons ]; then \
	  mkdir -p $(DESTDIR)$(HOPSHAREDIR)/icons && \
	    chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR)/icons; \
        fi && \
        for p in $(ICONS:%=icons/%); do \
	  if [ ! -d $(DESTDIR)$(HOPSHAREDIR)/`dirname $$p` ]; then \
            mkdir -p $(DESTDIR)$(HOPSHAREDIR)/`dirname $$p` && \
	    chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR)/`dirname $$p`; \
          fi && \
	  $(INSTALL) $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$$p && \
	  chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$$p; \
        done
	if [ ! -d $(DESTDIR)$(HOPSHAREDIR)/buttons ]; then \
	  mkdir -p $(DESTDIR)$(HOPSHAREDIR)/buttons && \
	    chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR)/buttons; \
        fi && \
        for p in $(BUTTONS:%=buttons/%); do \
	  $(INSTALL) $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$$p && \
	  chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$$p; \
        done
	if [ ! -d $(DESTDIR)$(HOPSHAREDIR)/flash ]; then \
	  mkdir -p $(DESTDIR)$(HOPSHAREDIR)/flash && \
	    chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR)/flash; \
        fi && \
        for p in $(FLASHOBJS:%=flash/%); do \
	  $(INSTALL) $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$$p && \
	  chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$$p; \
        done && \
	for p in $(FLASHOPT:%=flash/%); do \
	  if [ -f $$p ]; then \
            $(INSTALL) $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$$p && \
            chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$$p; \
          fi; \
        done
	if [ ! -d $(DESTDIR)$(HOPSHAREDIR)/editor ]; then \
	  mkdir -p $(DESTDIR)$(HOPSHAREDIR)/editor && \
	    chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR)/editor; \
        fi && \
	for p in $(BUILDSHAREDIR)/editor/*; do \
          $(INSTALL) $$p $(DESTDIR)$(HOPSHAREDIR)/editor && \
          chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/editor/`basename $$p`; \
        done

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/.afile && \
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/icons && \
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/flash && \
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/buttons && \
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/editor && \
        for p in $(EXTRA); do \
	  /bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/$(EXTRA); \
        done
	for p in $(RTS) hop-autoconf runtime hop; do \
	  /bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/$$p; \
	  /bin/rm -f $(DESTDIR)$(HOPSHAREDIR)/$$p.gz; \
        done

#*---------------------------------------------------------------------*/
#*    hop.js ...                                                       */
#*---------------------------------------------------------------------*/
hop.js: hop-autoconf.js ../scheme2js/runtime/runtime.js hop-exception.js $(RTS) 
	echo "/* Automatically generated file (don't edit) */" > $@
	echo "/* Copyright Manuel Serrano, see Hop's LICENSE file */" >> $@
	echo "/* " >> $@
	date >> $@
	echo " */" >> $@
	if [ "$(DEBUG) " != "yes " ]; then \
	  cat $^ | sed -n '1h;1!H;$${;g;s%/[*]\([^*]\|[*][^/]\)*[*]/%%g;p;}' \
                 | sed -e "s|//[^\"]*$$||g" \
                 | sed -e "/^[ \t]*$$/D" \
                 >> $@; \
        else \
	  cat $^ >> $@; \
        fi

#*---------------------------------------------------------------------*/
#*    hop-runtime.sch ...                                              */
#*---------------------------------------------------------------------*/
EXPORTED_RT_JS = $(RTS) $(filter %.js,$(EXTRA)) hop-autoconf.js

SCHEME2JS_RUNTIME = ../scheme2js/runtime
EXPORTER = $(SCHEME2JS_RUNTIME)/exporter
EXPORTER_FLAGS = --module "__hop" --constant

hop-runtime.sch: $(EXPORTER) $(EXPORTED_RT_JS)
	@ $(EXPORTER) $(EXPORTER_FLAGS) -o $@ $(EXPORTED_RT_JS)

#*---------------------------------------------------------------------*/
#*    .afile ...                                                       */
#*---------------------------------------------------------------------*/
.afile: hop-runtime.sch $(filter %.scm,$(EXTRA)) $(filter %.sch,$(EXTRA))
	@ $(AFILE) -o $@ $^

#*---------------------------------------------------------------------*/
#*    Flash compilation                                                */
#*---------------------------------------------------------------------*/
flash/%.swf: flash/%.as
	(cd flash && $(FLASHCC) $(FFLAGS) $(@F) $(<F))

#*---------------------------------------------------------------------*/
#*    JavaScript compilation                                           */
#*---------------------------------------------------------------------*/
hop-exception.js: hop-exception.scm
	(LD_LIBRARY_PATH=$(BUILDLIBDIR):$$LD_LIBRARY_PATH; \
         export LD_LIBRARY_PATH; \
         DYLD_LIBRARY_PATH=$(BUILDLIBDIR):$$DYLD_LIBRARY_PATH; \
         export DYLD_LIBRARY_PATH; \
         PATH=$(BUILDBINDIR):$(BUILDLIBDIR):$$PATH; \
         export PATH; \
	 $(call compile3b,HOPC,BJFLAGS,HFLAGS,BLFLAGS,$< -o $@ -c))

#*---------------------------------------------------------------------*/
#*    backends                                                         */
#*---------------------------------------------------------------------*/
.PHONY: native jvm

native:

jvm:
	$(MAKE) -s jvm-list SRC=.. DIR=share DEST=..

jvm-list:
	ls $(SRC)/$(DIR) > $(DEST)/$(DIR)/.list
	for p in `ls $(SRC)/$(DIR)`; do \
	  if [ -d $(SRC)/$(DIR)/$$p ]; then \
            $(MAKE) jvm-list SRC=$(SRC)/$(DIR) DIR=$$p DEST=$(DEST)/$(DIR) || (echo "*** ERROR: $$p"; exit 1); \
          fi \
        done

#*---------------------------------------------------------------------*/
#*    predistrib                                                       */
#*---------------------------------------------------------------------*/
predistrib: hop-exception.js

predistrib-clean:
	$(RM) hop-exception.js

#*---------------------------------------------------------------------*/
#*    cleaning                                                         */
#*---------------------------------------------------------------------*/
.PHONY: clean predistrib-clean devclean

clean:
	$(RM) hop.js

devclean:

distclean: clean
