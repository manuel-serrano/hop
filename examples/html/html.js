/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/html/html.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jun  4 09:19:16 2014                          */
/*    Last change :  Thu Jul  3 14:58:30 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    HTML new tag                                                     */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g html.js                                           */
/*    browser: http://localhost:8080/hop/html                          */
/*=====================================================================*/
var hop = require( "hop" );

service html() {
   return <HTML> {
      <HEAD> {
	 css: html.resource( "html.hss" )
      },
      <BOX> {
	 id: "box1",
	 <BOX> {
	    id: "box2",
	    title: "box2",
	    "data-count": 2,
	    "foo"
	 },
	 <BOX> {
	    id: "box3",
	    title: "box3",
	    class: "clickable",
	    "data-count": 3,
	    onclick: ~{ alert( "box3 clicked" ); },
	    "bar"
	 }
      }
   }
}

function BOX( attributes, n1 ) {
   var id = attributes.id;
   var klass = attributes.class;

   delete attributes.id;
   delete attributes.class;

   delete arguments[ 0 ];
   
   return <DIV> {
      "data-hss-tag": "box",
      id: id,
      class: klass,
      attributes,
      ~{ window.addEventListener(
	 "load",
	 function() {
	    var el = document.getElementById( ${id} );
	    el.innerHTML = el.innerHTML + "/" + el.getAttribute( "data-count" );
	 } ) },
      arguments
   }
}
	 
console.log( "Go to \"http://%s:%d/hop/html\"", hop.hostname, hop.port );
