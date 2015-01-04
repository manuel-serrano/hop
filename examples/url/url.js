/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/url/url.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Sun Dec 21 07:23:45 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Online translation example                                       */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g url.js                                            */
/*    browser: http://localhost:8080/hop/url                           */
/*=====================================================================*/
var hop = require( "hop" );

var url_base = "http://mymemory.translated.net/api/get";

function translateText( text, langpair ) {
   var url = url_base
      + "?q=" + escape( text )
      + "&langpair=" + (langpair ? langpair : "en|fr");

   return hop.withURL(
      url,
      function( o ) {
	 if( o.responseStatus === 200 ) {
	    var t = o.responseData.translatedText;
	    
	    return hop.charsetConvert( unescape( t ), "UTF-8" );
	 }
      }
   );
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
