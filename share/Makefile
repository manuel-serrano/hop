#*=====================================================================*/
#*    serrano/prgm/project/hop/1.9.x/share/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 25 07:19:48 2004                          */
#*    Last change :  Tue May  6 10:37:10 2008 (serrano)                */
#*    Copyright   :  2004-08 Manuel Serrano                            */
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
                  JavaScriptFlashGateway.swf JavaScriptFlashGateway.js \
                  hop-sound.swf 
FLASHSRC	= HopServevt.as HopAudio.as HopVideo.as
FLASH		= $(FLASHOBJS) $(FLASHSRC)

RTSSRC		= hopscheme.js hop-dom.js hop-event.js hop-serialize.js \
                  base64.js hop-request.js hop-history.js hop-lib.js

RTS		= hop-paned.css hop-sorttable.js hop-tree.js \
		  hop-error.hss hop-paned.js hop-table.js hop.css \
                  hop-fx.js \
                  md5.js \
		  hop-lframe.css hop-slider.css hop-tabslider.css \
                  hop-notepad.hss hop-slider.js hop-tabslider.js \
	 	  hop-notepad.js hop-sorttable.css hop-tree.css \
		  hop-foldlist.js hop-foldlist.css \
                  hop-window.js hop-window.hss \
                  hop-editor.js hop-editor.css \
                  hop-file.js hop-file.hss \
                  hop-fileselect.js hop-fileselect.css \
                  hop-tooltip.js hop-dashboard.js \
                  hop-prefs.js hop-prefs.hss \
                  hop-debug.js hop-debug.css \
		  hop-sound.js hop-sound.scm \
                  hop-audio.js hop-audio.hss \
                  hop-video.js hop-video.hss \
                  hop-login.js hop-login.hss \
                  hop-canvas.scm

ICONS_AUDIO	= mute.png pause.png playlist.png prefs.png stop.png \
		  next.png play.png podcast.png prev.png unmute.png

ICONS_PANED	= hcursor.png vcursor.png

ICONS_SLIDER	= cursor.png

ICONS_WINDOW	= bottom-left.png close.png shadow-e.png shadow-se.png \
                  title-left.png bottom-middle.png icon.png shadow-ne.png \
                  shadow-sw.png title.png bottom-right.png maximize.png \
                  shadow-s.png title-right.png

ICONS_TREE	= device.png folder-close.png joinbottom.png plus.png \
		  empty.png folder-open.png minus.png plusbottom.png \
		  file.png join.png minusbottom.png vline.png

ICONS_FOLDLIST	= triangle-down.png triangle-right.png

ICONS_LOGIN	= login.png

ICONS_FCHOOSER	= cancel.png open.png run.png \
                  folder-close.png file.png home.png hdd.png desktop.png \
                  keyboard.png drag.png \
                  edit-add.png edit-remove.png

ICONS		= bookcase.png important.png shadow-se.png \
		  busy-anim-16.gif logo-bg.png shadow-sw.png \
		  dashboard.png notfound.png source.png \
		  dict.png resize-grip.png stop.png \
		  error.png shadow-e.png timeout.png \
		  error2.png shadow-ne.png warning.png \
		  hop-16x16.png hop.png hop-128x128.png \
                  shadow-s.png window-handle.png \
		  $(ICONS_AUDIO:%=hop-audio/%) \
                  $(ICONS_PANED:%=hop-paned/%) \
                  $(ICONS_SLIDER:%=hop-slider/%) \
                  $(ICONS_WINDOW:%=hop-window/%) \
                  $(ICONS_TREE:%=hop-tree/%) \
                  $(ICONS_FOLDLIST:%=hop-foldlist/%) \
                  $(ICONS_LOGIN:%=hop-login/%) \
                  $(ICONS_FCHOOSER:%=hop-filechooser/%)

BUTTONS		= cc.png firefox.png hop2.png inria.png opera.png \
		  css.png freebsd.png html401.png inria2.png paypal.png \
		  debian.png gpl.png ie1.png konqueror.png safari.png \
		  donate.png hop.png ie2.png mozilla.png xhtml.png rss.png
 
#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= $(RTSSRC) $(RTS) \
                  $(ICONS:%=icons/%) \
                  $(BUTTONS:%=buttons/%) \
                  editor \
		  $(FLASH:%=flash/%) \
                  Makefile

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
build: hop.js $(FLASHSRC:%.as=flash/%.swf) hop-runtime.sch $(BACKEND)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)$(HOPSHAREDIR) && \
	install -m a+r $(BUILDSHAREDIR)/hop.js $(DESTDIR)$(HOPSHAREDIR)/hop.js; \
	install -m a+r $(BUILDSHAREDIR)/hop-runtime.sch $(DESTDIR)$(HOPSHAREDIR)/hop-runtime.sch; \
        for p in $(RTS); do \
	  install -m a+r $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$$p; \
        done
	mkdir -p $(DESTDIR)$(HOPSHAREDIR)/icons && \
	chmod a+rx $(DESTDIR)$(HOPSHAREDIR)/icons && \
        for p in $(ICONS:%=icons/%); do \
          mkdir -p $(DESTDIR)$(HOPSHAREDIR)/`dirname $$p` && \
	  chmod a+rx $(DESTDIR)$(HOPSHAREDIR)/`dirname $$p` && \
	  install -m a+r $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$$p; \
        done
	mkdir -p $(DESTDIR)$(HOPSHAREDIR)/buttons && \
	chmod a+rx $(DESTDIR)$(HOPSHAREDIR)/buttons && \
        for p in $(BUTTONS:%=buttons/%); do \
	  install -m a+r $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$$p; \
        done
	mkdir -p $(DESTDIR)$(HOPSHAREDIR)/flash && \
	chmod a+rx $(DESTDIR)$(HOPSHAREDIR)/flash && \
        for p in $(FLASHOBJS:%=flash/%); do \
	  install -m a+r $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$$p; \
        done
	mkdir -p $(DESTDIR)$(HOPSHAREDIR)/editor && \
	chmod a+rx $(DESTDIR)$(HOPSHAREDIR)/editor && \
	install -m a+r $(BUILDSHAREDIR)/editor/* $(DESTDIR)$(HOPSHAREDIR)/editor
	- $(GZIP) $(BUILDSHAREDIR)/hop.js -c > $(HOPSHAREDIR)/hop.js.gz 2> /dev/null

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/icons && \
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/flash && \
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/buttons && \
	/bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/editor && \
        for p in $(RTS); do \
	  /bin/rm -rf $(DESTDIR)$(HOPSHAREDIR)/$(RTS); \
        done

#*---------------------------------------------------------------------*/
#*    hop.js ...                                                       */
#*---------------------------------------------------------------------*/
hop.js: hop-autoconf.js \
        ../scheme2js/runtime/runtime.js \
        $(RTSSRC)
	echo "/* Automatically generated file (don't edit) */" > $@
	if [ "$(DEBUG) " != "yes " ]; then \
	  cat $^ | sed -e "s%/[*]\([^*]\|[*][^/]\)*[*]/%%g" \
                       -e "s|^[ \t]+$$||g" \
                 >> $@; \
        else \
	  cat $^ >> $@; \
        fi


SCHEME2JS_RUNTIME = ../scheme2js/runtime
include $(SCHEME2JS_RUNTIME)/Makefile.include
EXPORTER = $(SCHEME2JS_RUNTIME)/exporter
EXPORTER_FLAGS = --module "hop-runtime" --constant
EXPORTED_RTS = $(RTSSRC) $(filter %.js,$(RTS)) hop-autoconf.js

hop-runtime.sch: $(EXPORTER) $(EXPORTED_RTS)
	$(EXPORTER) $(EXPORTER_FLAGS) -o $@ $(EXPORTED_RTS)

#*---------------------------------------------------------------------*/
#*    Flash compilation                                                */
#*---------------------------------------------------------------------*/
flash/%.swf: flash/%.as
	(cd flash && $(FLASHCC) $(FFLAGS) $(@F) $(<F))

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

