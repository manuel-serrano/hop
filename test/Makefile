#*=====================================================================*/
#*    serrano/prgm/project/hop/3.0.x/test/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jan 20 13:46:40 2006                          */
#*    Last change :  Wed Sep  9 10:32:56 2015 (serrano)                */
#*    Copyright   :  2006-15 Manuel Serrano                            */
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
  nodejs/BLACKLIST \
  nodejs/README \
  nodejs/TEST.in \
  nodejs/HOP.exe.in \
  nodejs/log \
  test262/BLACKLIST \
  test262/README \
  test262/TEST \
  hopjs/README \
  hopjs/TEST \
  hopjs/passwd.hop \
  hopjs/noserv/array.js \
  hopjs/noserv/comprehension.js \
  hopjs/noserv/ecma51.js \
  hopjs/noserv/number.js \
  hopjs/noserv/regexp.js \
  hopjs/noserv/unicode.js \
  hopjs/noserv/bufferrw.js \
  hopjs/noserv/json.js \
  hopjs/noserv/property.js \
  hopjs/noserv/string.js \
  hopjs/noserv/date.js \
  hopjs/noserv/require.js \
  hopjs/noserv/es6-let.js \
  hopjs/noserv/es6-let-tl.js \
  hopjs/noserv/es6-sym.js \
  hopjs/noserv/es6-template.js \
  hopjs/noserv/es6-promise.js \
  hopjs/noserv/es6-arrow.js \
  hopjs/noserv/es6-defval.js \
  hopjs/serv/serialization.js \
  hopjs/serv/service.js \
  hopjs/serv/service2.js \
  hopjs/serv/post.js \
  hopjs/serv/svc_args.js \
  hopjs/serv/authentication.js \
  hopjs/serv/authentication.js.rc.js \
  hopjs/serv/unregisterService.js

all:
	(cd hopjs; ./TEST)
