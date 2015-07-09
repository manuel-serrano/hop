/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/url/url.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Thu Jul  9 11:56:51 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Online translation example                                       */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g url.js                                            */
/*    browser: http://localhost:8080/hop/url                           */
/*=====================================================================*/
var hop = require( "hop" );

var svc = hop.webService( "http://mymemory.translated.net/api/get" );

function translateText( text, lang ) {
   var o = svc( { q: text, langpair: (lang ? lang : "en|fr") } ).postSync();
   
   if( o.responseStatus === 200 ) {
      var t = o.responseData.translatedText;
      
      return hop.charsetConvert( unescape( t ), "UTF-8" );
   }
}

service url() {
   var output = <DIV> {};
   var input = <INPUT> { value: "toto n'est pas content" };
   var select = <SELECT> {
      <OPTION> { label: "fr->en", value: "fr|en", "fr-&gt;en" },
      <OPTION> { label: "en->fr", value: "en|fr", "en-&gt;fr" }
   };
      
   var translate = service( text, langpair ) {
      return translateText( text, langpair );
   };
      
   return <HTML> {
      select,
      input,
      <BUTTON> {
	 onclick: ~{
	    ${translate}( ${input}.value, ${select}.value )
	       .post( function( v ) { ${output}.innerHTML = v; } )
	 },
	 "translate"
      } </BUTTON>,
      output
   };
}

console.log( "Go to \"http://%s:%d/hop/url\"", hop.hostname, hop.port );
