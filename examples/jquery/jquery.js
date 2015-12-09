/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/jquery/jquery.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:10 2014                          */
/*    Last change :  Wed Dec 17 17:33:07 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Jquery plugin example                                            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g jquery.js                                         */
/*    browser: http://localhost:8080/hop/jquery                        */
/*=====================================================================*/
service jquery() {
   return <html>
      <head title="jQuery UI Datepicker - Default functionality"
	    css=${[ "http://jqueryui.com/resources/demos/style.css",
		    "http://code.jquery.com/ui/1.10.4/themes/smoothness/jquery-ui.css" ]}
	    jscript=${[ "http://code.jquery.com/jquery-1.10.2.js",
			"http://code.jquery.com/ui/1.10.4/jquery-ui.js" ]}/>
      <p>Date: <datepicker/></p>
   </html>
}

function DATEPICKER( attrs ) {
   var id = attrs.id || "datepicker";
   return [
      ~{ $(function() { $("#" + ${id}).datepicker() }) },
      <input type="text" id=${id}/>
   ]
}

console.log( "Go to \"http://%s:%d/hop/jquery\"", hop.hostname, hop.port );
