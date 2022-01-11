/*=====================================================================*/
/*    serrano/prgm/project/hop/3.5.x/doc/node.js                       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  4 17:59:11 2016                          */
/*    Last change :  Thu Jan  6 16:55:51 2022 (serrano)                */
/*    Copyright   :  2016-22 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Generate the NODE index                                          */
/*=====================================================================*/
"use hopscript";

const node = "https://nodejs.org/api";

/*---------------------------------------------------------------------*/
/*    stdlib                                                           */
/*---------------------------------------------------------------------*/
const stdlib = [ "console", "buffer", "child_process", "cluster",
		 "crypto", "dns", "domain", "events", "fs", "globals",
		 "http", "https", "modules", "net", "os", "path",
		 "process", "punycode", "querystring", "stream",
		 "string_decoder", "timers", "tls", "tty", "dgram",
		 "url", "util", "vm" ];

/*---------------------------------------------------------------------*/
/*    getChapterBindings ...                                           */
/*---------------------------------------------------------------------*/
function getChapterBindings( chapter, _i, _arr ) {

   function getEntry( entry ) {
      const a = entry.childNodes[ 0 ];
      const proto = a.innerHTML;

      const i = proto.indexOf( "." );
      const k = i >= 0 ? proto.substring( i + 1 ) : proto;
      const p = k.indexOf( "(" );

      const key = p >= 0 ? k.substring( 0, p ) : k;

      return {
	 key: key,
	 proto: proto,
	 chapter: "node",
	 type: p >= 0 ? "function" : "parameter",
	 url: chapter + ".html" + a.href
      };
   }

   function getSectionProtos( nodes ) {
      let arr = [];
      
      for( let i = 0; i < nodes.length; i++ ) {
	 if( typeof nodes[ i ] == "xml-element" ) {
	    if( nodes[ i ].tagName == "dt" ) {
	       const el = getEntry( nodes[ i ] );
	       if( el ) arr.push( el );
	    }
	 }
      }

      return arr;
   }
      
   const url = node + "/" + chapter + ".html";
   const html = require( url, "html" );

   // get the properties
   const toc = html.getElementById( "toc" );
   const lis = toc
	 .getElementsByTagName( "li" )
	 .filter( e => e.childNodes.length == 1
		  && e.childNodes[ 0 ].tagName === "a"
		  && e.childNodes[ 0 ].childNodes.length === 1
		  && e.childNodes[ 0 ].innerHTML.match( /^[a-z][a-zA-Z0-9]+[.]/ ) );

   console.error( url );
   return lis.map( getEntry );
}

/*---------------------------------------------------------------------*/
/*    Index generator                                                  */
/*---------------------------------------------------------------------*/
const arrs = stdlib.map( getChapterBindings );

console.log( JSON.stringify( [].concat.apply( [], arrs ) ) );
			  
