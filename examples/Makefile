#*=====================================================================*/
#*    serrano/prgm/project/hop/3.1.x/examples/Makefile                 */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 13:46:40 2006                          */
#*    Last change :  Sat Oct 29 08:41:00 2016 (serrano)                */
#*    Copyright   :  2006-16 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Example Makefile                                                 */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
-include ../etc/Makefile.hopconfig
-include ../etc/Makefile.version

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION = Makefile \
  README.md \
  async/async.js async/example.json async/README.md \
  arraybuffer/arraybuffer.js arraybuffer/example.json \
  require/node_modules require/node_modules/mod2.js require/require.js \
  require/mod1.js require/example.json \
  requirec/requirec.js requirec/mod2.js requirec/mod1.js \
  requirec/example.json requirec/README.md \
  requirew/requirew.js requirew/provider.js requirew/foo.js \
  requirew/example.json \
  js2http/js2http.js js2http/fact.js \
  arraybuffer/arraybuffer.js \
  qunit/qunit.js qunit/example.json \
  event/event.js event/example.json \
  fs/fs.js fs/example.json \
  url/url.js url/example.json url/README.md \
  file/file.js file/example.json file/README.md \
  websocket/wsserver.js websocket/wsclient.js websocket/example.json \
  websocket/README.md \
  wspost/wsserver.js wspost/wsclient.js wspost/example.json \
  wspost/README.md \
  wsclient/wsclient.js wsclient/example.json \
  svc/svc.js svc/example.json svc/README.md \
  svc1/svc1.js svc1/example.json svc1/README.md \
  svc2/svc2.js svc2/extern.js svc2/example.json svc2/README.md \
  svc3/svc3.js svc3/example.json svc3/README.md \
  svc4/svc4.js svc4/example.json \
  html/html.js html/html.hss html/example.json \
  htmlc/htmlc.js htmlc/example.json \
  htmlr/htmlr.js htmlr/example.json htmlr/htmlr.html htmlr/htmlr.hss \
  htmlr/README.md \
  hss/hss.hss hss/hss.js hss/example.json hss/README.md \
  tree/tree.js tree/example.json tree/README.md \
  spage/spage.js spage/example.json spage/README.md \
  tower/tower.js tower/example.json \
  dom/dom.js dom/example.json dom/README.md \
  jquery/jquery.js jquery/example.json \
  hello/hello.js hello/example.json hello/README.md \
  svg/svg.js svg/example.json svg/README.md \
  pdfviewer/pdfviewer.js pdfviewer/example.json pdfviewer/README.md \
  pdfviewer/pdfwrap.js pdfviewer/pdf.js pdfviewer/example.pdf \
  debug/debug.js debug/bugs.js debug/example.json \
  proxy/proxy.js proxy/example.json \
  worker/slave.js worker/worker.js worker/bar.js worker/example.json worker/README.md \
  worker2/slave.js worker2/worker2.js worker2/example.json worker2/README.md \
  worker3/slave.js worker3/worker3.js worker3/bar.js worker3/example.json \
  worker4/slave.js worker4/worker4.js worker4/example.json worker4/README.md \
  worker5/slave.js worker5/worker5.js worker5/example.json worker5/README.md \
  sqlite/db.sqlite sqlite/sqlite.js sqlite/sqlite_core.js sqlite/example.json \
  react/react.js react/README.md react/example.json \
  reactsrv/reactsrv.js reactsrv/README.md reactsrv/example.json \
  reactc/reactc.js reactc/README.md reactc/example.json \
  widget/node_modules widget/example.json \
  widget/node_modules/flipcounter \
  widget/node_modules/flipcounter/lib \
  widget/node_modules/flipcounter/lib/flipcounter.js \
  widget/node_modules/flipcounter/client \
  widget/node_modules/flipcounter/client/flipcounter.min.js \
  widget/node_modules/flipcounter/package.json \
  widget/node_modules/flipcounter/hss \
  widget/node_modules/flipcounter/hss/flipcounter.hss \
  widget/node_modules/flipcounter/etc \
  widget/node_modules/flipcounter/etc/img \
  widget/node_modules/flipcounter/etc/img/digits-transparent.png \
  widget/node_modules/flipcounter/etc/img/digits.png \
  widget/node_modules/flipcounter-1.0.0.tgz \
  widget/widget.js \
  custom/custom.js custom/README.md custom/example.json \
  custom/foo-bar.js custom/foo-bar.css \
  keepalive/keepalive.js keepalive/keepalive.hss keepalive/example.json \
  keepalive/README.md \
  authentication/authentication.js authentication/example.json \
  authentication/README.md \
  authentication2/authentication2.js authentication2/example.json \
  authentication2/passwd.js \
  authentication2/remote.js \
  image/image.js image/example.json image/README.md \
  pair/pair.js pair/example.json \
  hzdemo/hzdemo.js hzdemo/example.json \
  hzdemo/README \
  hzdemo/foo/package.json \
  hzdemo/foo/foo.js \
  hzdemo/foo/foo.hss \
  hzdemo/foo/etc \
  hzdemo/foo/etc/favicon.png \
  hzdemo/foo/etc/logo.png \
  hzdemo/foo-1.0.0.hz \
  scheme/scheme.js \
  scheme/scheme.hop \
  md/md.js md/markdown.hss md/tex.hss md/README.md md/example.json \
  mdtag/mdtag.js mdtag/README.md mdtag/example.json \
  syslog/README.md syslog/syslog.js syslog/example.json \
  examples/doc.js examples/doc.wiki examples/example.json examples/examples.hss \
  examples/examples.js examples/libs/bootstrap/css/bootstrap-theme.css \
  examples/libs/bootstrap/css/bootstrap-theme.css.map \
  examples/libs/bootstrap/css/bootstrap-theme.min.css \
  examples/libs/bootstrap/css/bootstrap.css \
  examples/libs/bootstrap/css/bootstrap.css.map \
  examples/libs/bootstrap/css/bootstrap.min.css \
  examples/libs/bootstrap/fonts/glyphicons-halflings-regular.eot \
  examples/libs/bootstrap/fonts/glyphicons-halflings-regular.svg \
  examples/libs/bootstrap/fonts/glyphicons-halflings-regular.ttf \
  examples/libs/bootstrap/fonts/glyphicons-halflings-regular.woff \
  examples/libs/bootstrap/js/bootstrap.js \
  examples/libs/bootstrap/js/bootstrap.min.js \
  examples/libs/bootstrap/js/npm.js \
  examples/libs/jquery/js/jquery.min.js \
  examples/passwd.hop \
  examples/src.js

#*---------------------------------------------------------------------*/
#*    The hop executable                                               */
#*---------------------------------------------------------------------*/
.PHONY: build clean install uninstall

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install:
	$(MAKE) mkdir DIR=$(DESTDIR)$(HOPDOCDIR)/examples
	chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/examples
	for p in *; do \
          if [ -d $p  ]; then \
             cp -r $p $(DESTDIR)$(HOPDOCDIR)/examples; \
             chmod $(MODDIR) $(DESTDIR)$(HOPDOCDIR)/examples/$p; \
             chmod $(MODFILE) $(DESTDIR)$(HOPDOCDIR)/examples/$p/*; \
          fi \
        done

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	$(RM) -rf $(DESTDIR)$(HOPDOCDIR)/examples

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:

#*---------------------------------------------------------------------*/
#*    devclean                                                         */
#*---------------------------------------------------------------------*/
devclean: clean

