/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/mdtag/mdtag.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Aug 23 08:07:57 2015                          */
/*    Last change :  Fri Oct 23 11:10:57 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    An example using the custom Html tag                             */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g mdtag.js                                          */
/*    browser: http://localhost:8080/hop/mdtag                         */
/*=====================================================================*/
var proto = Object.create( HTMLElement.prototype );

proto.createdCallback = function() {
   this.innerHTML = 'Hello, <b>' +
      (this.getAttribute('name') || '?') + '</b>';
};

document.registerElement( 'foo-bar', { prototype: proto } );

alert( "glop" );
