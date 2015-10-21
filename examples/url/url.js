/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/url/url.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Sun Oct 18 09:09:15 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Online translation example                                       */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g url.js                                            */
/*    browser: http://localhost:8080/hop/url                           */
/*=====================================================================*/
"use hopscript";

var hop = require( "hop" );

var mymemory = hop.webService( "http://mymemory.translated.net/api/get" );

function translateText( text, lang = "en|fr" ) {
   var o = mymemory( { q: text, langpair: lang } ).postSync();
   
   if( o.responseStatus === 200 ) {
      var t = o.responseData.translatedText;
      
      return hop.charsetConvert( unescape( t ), "UTF-8" );
   }
}

service url() {
   var output = <div/>;
   var input = <input value="toto n'est pas content"/>;
   var select = <select>
     <option label="fr->en" value="fr|en">fr-&gt;en</option>
     <option label="en->fr" value="en|fr">en-&gt;fr</option>
   </select>
      
   var translate = service( text, langpair ) {
      return translateText( text, langpair );
   };
      
   return <html>
     <div>
       ${select}
       ${input}
       <button onclick=~{
	  ${translate}( ${input}.value, ${select}.value )
	     .post( function( v ) { ${output}.innerHTML = v; } )}>
         translate
       </button>
       ${output}
     </div>
     </html>;
}

console.log( "Go to \"http://%s:%d/hop/url\"", hop.hostname, hop.port );
