/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/jquery/jquery.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:10 2014                          */
/*    Last change :  Thu Jul  3 15:00:01 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Jquery plugin example                                            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g jquery.js                                         */
/*    browser: http://localhost:8080/hop/jquery                        */
/*=====================================================================*/
var hop = require( 'hop' );

service jquery() {
   return <HTML> {
      <HEAD> {
	 <TITLE> { "jQuery UI Datepicker - Default functionality" },
	 <LINK> {
	    rel: "stylesheet",
	    href: "http://code.jquery.com/ui/1.10.4/themes/smoothness/jquery-ui.css"
	 },
	 <SCRIPT> { src: "http://code.jquery.com/jquery-1.10.2.js" },
	 <SCRIPT> { src: "http://code.jquery.com/ui/1.10.4/jquery-ui.js" },
	 <LINK> {
	    rel: "stylesheet",
	    href: "http://jqueryui.com/resources/demos/style.css"
	 }
      },
      <P> {
	 "Date: ", <DATEPICKER> {}
      }
   }
}

service jquery2() {
   return <HTML> {
      <HEAD> {
	 title: "jQuery UI Datepicker - Default functionality",
	 css: [ jquery.resource( "iframe.hss" ),
		"http://jqueryui.com/resources/demos/style.css",
		"http://code.jquery.com/ui/1.10.4/themes/smoothness/jquery-ui.css" ],
	 jscript: [ "http://code.jquery.com/jquery-1.10.2.js",
		    "http://code.jquery.com/ui/1.10.4/jquery-ui.js" ],
      },
      <P> {
	 "Date: ", <DATEPICKER> {}
      }
   }
}

function DATEPICKER( attrs ) {
   var id = attrs.id || "datepicker";
   return [
      ~{ $(function() { $("#" + ${id}).datepicker() }) },
      <INPUT> {
	 type: "text",
	 id: id
      }
   ]
}

console.log( "Go to \"http://%s:%d/hop/jquery\"", hop.hostname, hop.port );
