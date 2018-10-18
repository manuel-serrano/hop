/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/syslog/syslog.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Aug 23 08:07:57 2015                          */
/*    Last change :  Wed May 17 14:22:18 2017 (serrano)                */
/*    Copyright   :  2015-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    An example of SYSLOG messaging                                   */
/*    -------------------------------------------------------------    */
/*    run: hop --no-server -v -g syslog.js                             */
/*=====================================================================*/

var Syslog = require( hop.syslog );

Syslog.open( "hopjs-syslog", Syslog.LOG_PID | Syslog.LOG_ODELAY, Syslog.LOG_LOCAL0 );

Syslog.log( Syslog.LOG_INFO, "A hop.js message [v" + process.versions.hop + "]" ) ;

Syslog.close();

console.log( "check your log file (typically /var/log/syslog)" );
