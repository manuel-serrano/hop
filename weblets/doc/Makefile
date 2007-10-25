#*=====================================================================*/
#*    serrano/prgm/project/hop/weblets/doc/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 14:35:57 2006                          */
#*    Last change :  Thu Oct 18 13:28:26 2007 (serrano)                */
#*    Copyright   :  2006-07 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile of the DOC weblet                                   */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../etc/Makefile.hopconfig
include ../../etc/Makefile.version

HOP_POP         = doc.hop config.hop \
                  api.hop tutorials.hop libraries.hop weblets.hop \
                  search.hop widgets.hop wiki.hop services.hop index.hop
API_POP		= access.wiki base64.wiki cache.wiki cgi.wiki config.wiki \
                  cookie.wiki delay.wiki dom.wiki editor.wiki exception.wiki \
                  fileselect.wiki filters.wiki foldlist.wiki head.wiki \
                  hooks.wiki hss.wiki html.wiki http-message.wiki \
                  http-request.wiki http-response.wiki http.wiki \
                  img.wiki inline.wiki md5.wiki notepad.wiki \
                  pan.wiki param-misc.wiki param-rc.wiki parameter.wiki \
                  read.wiki service.wiki slider.wiki sorttable.wiki \
                  sound.wiki sqlite.wiki tabslider.wiki timeout.wiki \
                  tooltip.wiki tree.wiki url.wiki wiki.wiki window.wiki \
                  xml.wiki symbol.wiki rss.wiki history.wiki \
                  authentication.wiki regexp.wiki format.wiki webdav.wiki \
                  charset.wiki
CORE_POP	= core.wiki
EXAMPLES_POP	= base64.hop canvas.hop editor-form.hop editor.hop \
                  filebrowse.hop filebrowse2.hop fileselect.hop \
                  filter-1.hop filter-2.hop \
                  foldlist-1.hop foldlist-2.hop hook-1.hop inline.hop \
                  iwindow.hop notepad-1.hop notepad-2.hop paned-1.hop \
                  paned-2.hop parameter.hop request-1.hop rss.hop \
                  service-1.hop service-2.hop service-3.hop service-4.hop \
                  service-5.hop service-6.hop slider-1.hop slider-2.hop \
                  sorttable.hop sound-1.hop sound-2.hop sound-3.hop \
                  tabslider-1.hop tooltip-1.hop tree-1.hop tree-2.hop \
                  wiki.hop wiki2.hop window.hop history-1.hop history-2.hop \
                  cookie.hop webdav.hop
WEBLETS_POP	= .keep
WIKI_POP	= basic.wiki hop.wiki link.wiki lists.wiki misc.wiki \
                  paragraph.wiki plugin.wiki section.wiki syntax.wiki \
                  tables.wiki tt.wiki
TUTORIAL_POP	= 00-hello.wiki 01-ajax.wiki 02-hss.wiki 03-proxy.wiki \
		  04-authentication.wiki 05-rss.wiki 06-hz.wiki \
                  07-hoprc.wiki 08-history.wiki 09-resource.wiki \
                  10-javascript.wiki

POPULATION	= Makefile .afile doc.hss doc.scm \
                  examples api core wiki tutorials etc/weblet.info \
                  etc/logo.png etc/doc.wiki \
                  etc/sound1.mp3 etc/sound2.mp3 \
                  $(HOP_POP) \
		  $(API_POP:%=api/%) \
		  $(CORE_POP:%=core/%) \
		  $(EXAMPLES_POP:%=examples/%) \
		  $(WEBLETS_POP:%=weblets/%) \
		  $(WIKI_POP:%=wiki/%) \
		  $(TUTORIAL_POP:%=tutorials/%)

build: .afile

.afile: $(HOP_POP)
	$(AFILE) -suffix hop $(HOP_POP) > .afile
