/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/custom/foo-bar.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Aug 23 08:07:57 2015                          */
/*    Last change :  Mon Jul 10 08:29:47 2017 (serrano)                */
/*    Copyright   :  2015-17 Manuel Serrano                            */
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
