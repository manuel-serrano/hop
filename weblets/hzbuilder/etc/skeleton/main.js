
var config = require( "./config.js" );

service @HZ@() {
   return <HTML> {
      <HEAD> {
	 title: "@TITLE@",
	 favicon: @HZ@.resource( "etc/favicon.png" ),
	 css: @HZ@.resource( "@HZ@.hss" )
      },
      <BODY> {
	 <TABLE> {
	    <TH> { "version" }, <TD> { config.version },
	    <TH> { "date" }, <TD> { config.date },
	    <TH> { "author" }, <TD> { config.date },
	 }
      }
   }
}
	 
