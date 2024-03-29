#*=====================================================================*/
#*    serrano/prgm/project/hop/hop/share/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 25 07:19:48 2004                          */
#*    Last change :  Sun Oct 16 13:31:26 2022 (serrano)                */
#*    Copyright   :  2004-22 Manuel Serrano                            */
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
-include $(BIGLOOLIBDIR)/Makefile.config
-include $(BIGLOOLIBDIR)/Makefile.misc

# use to mark js file to avoir browser cache effects
JS_STAMP = $(HOPRELEASE)

#*---------------------------------------------------------------------*/
#*    Client-side runtime library                                      */
#*    -------------------------------------------------------------    */
#*    See hopscheme/hop-runtime.scm to add new modules.                */
#*---------------------------------------------------------------------*/
EXTRA		= hop.hss \
                  hop-fx.js hop-fx.hss \
                  hop-paned.hss hop-paned.js \
                  hop-sorttable.hss hop-sorttable.js \
                  md5.js sha1.js aes.js aes.scm \
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
                  hop-canvas.scm hop-exception.scm hop-debug.scm \
                  hop-orientation.scm \
                  hop-spage.scm hop-spage.hss hop-spage-iphone.hss \
                  hopjs-spage.js \
                  hop-gauge.scm hop-gauge.hss \
                  hop-font.scm \
                  hop-password.scm \
                  hop-foot.hss \
                  hop-nodejs.js base64.js hop-history.js
EXTRA_JS	= $(filter %.js,$(EXTRA))
EXTRA_NOJS 	= $(filter-out %.js,$(EXTRA))

#*---------------------------------------------------------------------*/
#*    AUDIO and ICONS                                                  */
#*---------------------------------------------------------------------*/
ICONS_HOP	= logo-bg.png dashboard.png \
		  hop-16x16.png hop-48x48.png hop-32x32.png hop-128x128.png \
                  hop-api-48x48.png hop-api-16x16.png hop.png hop.svg \
                  inria.svg favicon-16x16.png

ICONS_ERRORS    = error.png error2.png client-error.png stop.png timeout.png \
                  warning.png privacy.png notfound.png

ICONS_ANIMS	= busy-anim-16.gif busy-anim-32.gif

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

ICONS_LOGIN	= login.png

ICONS_FCHOOSER	= folder-close.png file.png home.png hdd.png desktop.png \
                  keyboard.png drag.png \
                  edit-add.png edit-remove.png

ICONS_COLOR	= transbg.png satval.png 

ICONS_ACTIONS   = cancel.png ok.png run.png open.png

ICONS		= $(ICONS_HOP:%=hop/%) \
                  $(ICONS_ERRORS:%=errors/%) \
                  $(ICONS_ANIMS:%=anims/%) \
		  $(ICONS_AUDIO:%=hop-audio/%) \
                  $(ICONS_PANED:%=hop-paned/%) \
                  $(ICONS_SLIDER:%=hop-slider/%) \
                  $(ICONS_WINDOW:%=hop-window/%) \
                  $(ICONS_WINDOW_RED:%=hop-window-red/%) \
                  $(ICONS_FX:%=hop-fx/%) \
                  $(ICONS_TREE:%=hop-tree/%) \
                  $(ICONS_LOGIN:%=hop-login/%) \
                  $(ICONS_FCHOOSER:%=hop-filechooser/%) \
                  $(ICONS_COLOR:%=hop-color/%) \
                  $(ICONS_ACTIONS:%=actions/%)

BUTTONS		= cc.png firefox.png hop2.png inria.png opera.png \
		  css.png freebsd.png html401.png inria-2010.png paypal.png \
		  debian.png gpl.png ie1.png konqueror.png safari.png \
		  donate.png hop.png ie2.png mozilla.png xhtml.png rss.png
 
#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= $(RTSCONFIG) $(EXTRA) \
                  $(ICONS:%=icons/%) \
                  $(BUTTONS:%=buttons/%) \
                  editor \
                  values.sch \
                  Makefile

#*---------------------------------------------------------------------*/
#*    build                                                            */
#*---------------------------------------------------------------------*/
build: .afile hop_u.js hop_s.js hopjs_u.js hopjs_s.js \
   $(BACKEND) hop-password.scm

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
	mkdir -p $(DESTDIR)$(HOPSHAREDIR)
	mkdir -p $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)
	chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR)
	chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)
	$(INSTALL) $(BUILDSHAREDIR)/.afile $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/.afile && \
	chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/.afile && \
	$(INSTALL) $(BUILDSHAREDIR)/hop-runtime.sch $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/hop-runtime.sch && \
	chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/hop-runtime.sch && \
        for p in hop_u hop_s hopjs_u hopjs_s hop-autoconf hop-react $(RTSCONFIG:%.js=%); do \
	  $(INSTALL) $(BUILDSHAREDIR)/$$p.js $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p-$(JS_STAMP).js && \
	  chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p-$(JS_STAMP).js && \
	  $(GZIP) $(BUILDSHAREDIR)/$$p.js -c > $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p-$(JS_STAMP).js.gz 2> /dev/null && \
          chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p-$(JS_STAMP).js.gz; \
        done
	for p in $(EXTRA_JS:%.js=%); do \
	  $(INSTALL) $(BUILDSHAREDIR)/$$p.js $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p.js && \
	  chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p.js && \
	  $(GZIP) $(BUILDSHAREDIR)/$$p.js -c > $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p.js.gz 2> /dev/null && \
          chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p.js.gz; \
        done
	$(INSTALL) ../scheme2js/runtime/runtime_u.js $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/runtime_u-$(JS_STAMP).js && \
	chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/runtime_u-$(JS_STAMP).js && \
	$(INSTALL) ../scheme2js/runtime/runtime_u.js $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/runtime_u-$(JS_STAMP).js && \
	$(GZIP) ../scheme2js/runtime/runtime_u.js -c > $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/runtime_u-$(JS_STAMP).js.gz && \
        chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/runtime_u-$(JS_STAMP).js.gz; \
	$(INSTALL) ../scheme2js/runtime/runtime_s.js $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/runtime_s-$(JS_STAMP).js && \
	chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/runtime_s-$(JS_STAMP).js && \
	$(INSTALL) ../scheme2js/runtime/runtime_s.js $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/runtime_s-$(JS_STAMP).js && \
	$(GZIP) ../scheme2js/runtime/runtime_s.js -c > $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/runtime_s-$(JS_STAMP).js.gz && \
        chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/runtime_s-$(JS_STAMP).js.gz; \
        for p in $(EXTRA_NOJS); do \
	  $(INSTALL) $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p && \
	  chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p; \
        done
	if [ ! -d $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/icons ]; then \
	  mkdir -p $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/icons && \
	    chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/icons; \
        fi && \
        for p in $(ICONS:%=icons/%); do \
	  if [ ! -d $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/`dirname $$p` ]; then \
            mkdir -p $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/`dirname $$p` && \
	    chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/`dirname $$p`; \
          fi && \
	  $(INSTALL) $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p && \
	  chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p; \
        done
	if [ ! -d $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/buttons ]; then \
	  mkdir -p $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/buttons && \
	    chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/buttons; \
        fi && \
        for p in $(BUTTONS:%=buttons/%); do \
	  $(INSTALL) $(BUILDSHAREDIR)/$$p $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p && \
	  chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p; \
        done
	if [ ! -d $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/editor ]; then \
	  mkdir -p $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/editor && \
	    chmod $(MODDIR) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/editor; \
        fi && \
	for p in $(BUILDSHAREDIR)/editor/*; do \
          $(INSTALL) $$p $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/editor && \
          chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/editor/`basename $$p`; \
        done
	$(INSTALL) ../docker/Dockerfile $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/Dockerfile.hop
	chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/Dockerfile.hop
	$(INSTALL) ../docker/hop.docker $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/hop.docker
	chmod $(MODFILE) $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/hop.docker

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	$(RM) -rf $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/.afile && \
	$(RM) -rf $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/icons && \
	$(RM) -rf $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/buttons && \
	$(RM) -rf $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/editor && \
        for p in $(EXTRA_NOJS); do \
	  $(RM) -rf $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p; \
        done
	for p in $(EXTRA_JS); do \
	  $(RM) -rf $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p; \
	  $(RM) -rf $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p.gz; \
        done
	for p in runtime_s runtime_u hop_u hop_s hopjs_u hopjs_s hop-autoconf hop-react $(RTSCONFIG:%.js=%); do \
	  $(RM) -f $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p-$(JS_STAMP).js; \
	  $(RM) -f $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/$$p-$(JS_STAMP).js.gz; \
        done
	$(RM) -f $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/hop-runtime.sch
	$(RM) -f $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/Dockerfile.hop
	$(RM) -f $(DESTDIR)$(HOPSHAREDIR)/$(HOPRELEASE)/hop.docker

#*---------------------------------------------------------------------*/
#*    hop.js ...                                                       */
#*---------------------------------------------------------------------*/
hop_u.js: hop-boot.js hop-autoconf.js ../scheme2js/runtime/runtime_u.js \
  hop-exception.js hop-nodejs.js hop-react.js \
  $(RTSCONFIG) base64.js hop-history.js hop-builtin-serializers.js \
  $(BGLCPP)
	echo "/* Automatically generated file (don't edit) */" > $@
	echo "/* Copyright Manuel Serrano, see Hop's LICENSE file */" >> $@
	date +'/* %D %I:%H */' >> $@
	echo >> $@
	echo "\"use strict\";" >> $@
	echo >> $@
	cat $(filter %.js,$^) \
               | sed -n '1h;1!H;$${;g;s%/[*]\([^*]\|[*][^/]\)*[*]/%%g;p;}' \
               | sed -e "s|//[^\"]*$$||g" \
               | sed -e "/^[ \t]*$$/D" \
               | $(BGLCPP) -DHOP_SCHEME\
               >> $@

hop_s.js: hop-boot.js hop-autoconf.js ../scheme2js/runtime/runtime_s.js \
  hop-exception.js hop-debug.js hop-nodejs.js hop-react.js \
  $(RTSCONFIG) base64.js hop-history.js hop-builtin-serializers.js \
  $(BGLCPP)
	echo "/* Automatically generated file (don't edit) */" > $@
	echo "/* Copyright Manuel Serrano, see Hop's LICENSE file */" >> $@
	date +'/* %D %I:%H */' >> $@
	echo >> $@
	echo "\"use strict\";" >> $@
	echo >> $@
	if [ "$(DEBUG) " != "yes " ]; then \
	  cat $(filter %.js,$^) \
                 | sed -n '1h;1!H;$${;g;s%/[*]\([^*]\|[*][^/]\)*[*]/%%g;p;}' \
                 | sed -e "s|//[^\"]*$$||g" \
                 | sed -e "/^[ \t]*$$/D" \
                 | $(BGLCPP) -DHOP_SCHEME -DHOP_RTS_DEBUG \
                 >> $@; \
        else \
	  cat $(filter %.js,$^) \
                 | sed -n '1h;1!H;$${;g;s%/[*]\([^*]\|[*][^/]\)*[*]/%%g;p;}' \
                 | sed -e "s|//[^\"]*$$||g" \
                 | sed -e "/^[ \t]*$$/D" \
                 | $(BGLCPP) -DHOP_SCHEME \
                 >> $@; \
        fi

#*---------------------------------------------------------------------*/
#*    hop.js ...                                                       */
#*---------------------------------------------------------------------*/
hopjs_u.js: hop-boot.js hop-autoconf.js $(RTSCONFIG) \
  hop-nodejs.js hop-react.js \
  hop-builtin-serializers.js \
  $(BGLCPP)
	echo "/* Automatically generated file (don't edit) */" > $@
	echo "/* Copyright Manuel Serrano, see Hop's LICENSE file */" >> $@
	date +'/* %D %I:%H */' >> $@
	echo >> $@
	echo "\"use strict\";" >> $@
	echo >> $@
	echo "var hop_current_stack_context;" >> $@
	echo "function hop_callback_listener_context(m) { return m; }" >> $@
	echo "function hop_callback_html_context(m) { return m; }" >> $@
	echo "function hop_callback_handler( e, ctx ) { throw e; }" >> $@
	echo "function sc_typeof(o) { return typeof(o); }" >> $@
	echo "function sc_error( proc, msg, obj ) { throw msg; }" >> $@
	cat $(filter %.js,$^) \
               | sed -n '1h;1!H;$${;g;s%/[*]\([^*]\|[*][^/]\)*[*]/%%g;p;}' \
               | sed -e "s|//[^\"]*$$||g" \
               | sed -e "/^[ \t]*$$/D" \
               | $(BGLCPP) -DHOP_JAVASCRIPT | $(CLOSURE) \
               >> $@
	echo "hop.sha1sum = (function() {" >> $@
	cat sha1.js >> $@
	echo "return sha1sum;" >> $@
	echo "})();" >> $@

hopjs_s.js: hop_s.js
	cp $^ $@
	echo "hop.sha1sum = (function() {" >> $@
	cat sha1.js >> $@
	echo "return sha1sum;" >> $@
	echo "})();" >> $@

#*---------------------------------------------------------------------*/
#*    hop-runtime.sch ...                                              */
#*---------------------------------------------------------------------*/
EXPORTED_RT_JS = $(RTSCONFIG) $(EXTRA_JS) hop-autoconf.js hop-react.js

SCHEME2JS_RUNTIME = ../scheme2js/runtime
EXPORTER = $(SCHEME2JS_RUNTIME)/exporter
EXPORTER_FLAGS = --module "__hop" --constant

hop-runtime.sch: $(EXPORTER) $(EXPORTED_RT_JS)
	$(EXPORTER) $(EXPORTER_FLAGS) -o $@ $(EXPORTED_RT_JS)

#*---------------------------------------------------------------------*/
#*    .afile ...                                                       */
#*---------------------------------------------------------------------*/
.afile: hop-runtime.sch $(filter %.scm,$(EXTRA)) $(filter %.sch,$(EXTRA))
	@ $(AFILE) -o $@ $^

#*---------------------------------------------------------------------*/
#*    JavaScript compilation                                           */
#*---------------------------------------------------------------------*/
hop-exception.js: hop-exception.scm ../runtime/client-exception.sch
	$(call compile3,$(HOPC),--no-use-strict --no-source-map $(BJFLAGS),$(HFLAGS),$(BLFLAGS),-c,$< -o $@)
	cat hop-exception-onload.js >> hop-exception.js

hop-debug.js: hop-debug.scm ../runtime/sourcemap.sch values.sch
	$(call compile3,$(HOPC),--no-use-strict --no-source-map $(BJFLAGS),$(HFLAGS),$(BLFLAGS),-c,$< -o $@)

hop-builtin-serializers.js: $(BUILDETCDIR)/mkserializer.hop
	($(call compile,$(BIGLOO),-suffix hop ,-L $(BUILDLIBDIR), -static-all-bigloo $<) && ./a.out > $@ && $(RM) -f a.out $(BUILDETCDIR)/mkserializer.o) || ($(RM) -f $@; exit 1)

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
predistrib: hop-exception.js hop-debug.js hop-builtin-serializers.js

predistrib-clean:
	$(RM) hop-exception.js hop-debug.js hop-builtin-serializers.js

#*---------------------------------------------------------------------*/
#*    cleaning                                                         */
#*---------------------------------------------------------------------*/
.PHONY: clean predistrib-clean devclean

clean:
	$(RM) -f hop_u.js hop_s.js 
	$(RM) -f hopjs_u.js hopjs_s.js 
	$(RM) -f hop-runtime.sch 

devclean:
	$(RM) hop-exception.js hop-debug.js hop-builtin-serializers.js

distclean: clean
