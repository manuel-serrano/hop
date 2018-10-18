/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/doc/mdn.js                        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  4 17:59:11 2016                          */
/*    Last change :  Wed Oct  5 17:54:30 2016 (serrano)                */
/*    Copyright   :  2016 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Generate MDN index                                               */
/*=====================================================================*/
"use hopscript";

const mdn = "https://developer.mozilla.org"
const globalObject = "/en-US/docs/Web/JavaScript/Reference/Global_Objects";
const api = "/docs/Web/API/";

/*---------------------------------------------------------------------*/
/*    stdlib                                                           */
/*---------------------------------------------------------------------*/
const stdlib = [ "Array", "Boolean", "Date", "Error", "Function",
		 "JSON", "Math", "Number", "Promise", "Regexp", "String" ];

/*---------------------------------------------------------------------*/
/*    getChapterBindings ...                                           */
/*---------------------------------------------------------------------*/
function getChapterBindings( chapter, _i, _arr ) {

   function findDL( node ) {
      if( !node ) {
	 return false;
      } else {
	 for( ; node; node = node.nextSibling ) {
	    if( typeof node == "xml-element" ) {
	       switch( node.tagName ) {
		  case "dl": return node;
		  case "div": return findDL( node.childNodes[ 0 ] );
	       }
	    }
	 }
      }
   }

   function getEntry( entry ) {
      const nodes = entry.childNodes;
      const a = nodes.find( (n, _i, __arr) => n.tagName == "a" );

      if( !a ) return false;
      
      const c = a.childNodes.find( (n, _i, __arr) => n.tagName == "code" );
      if( !c ) return false;

      var proto = c.innerHTML;

      const i = proto.lastIndexOf( "." );
      const k = i >= 0 ? proto.substring( i + 1 ) : proto;
      
      if( k == "prototoype" ) return false;
      
      const j = k.indexOf( "(" );

      return {
	 key: j >= 0 ? k.substring( 0, j ) : k,
	 proto: proto,
	 chapter: "mdn",
	 type: proto.indexOf( "(" ) >= 0 ? "function" : "parameter",
	 url: a.href
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
      
   const url = mdn + globalObject + "/" + chapter;
   const html = require( url, "html" );

   // get the properties
   const properties = findDL( html.getElementById( "Properties" ) );
   const properties2 = findDL( html.getElementById( "Properties_2" ) );
   const methods = findDL( html.getElementById( "Methods" ) );
   const methods2 = findDL( html.getElementById( "Methods_2" ) );
   const genmethods = findDL( html.getElementById( chapter + "_generic_methods" ) );

   return (properties ? getSectionProtos( properties.childNodes ): [] )
      .concat( properties2 ? getSectionProtos( properties2.childNodes ): [] )
      .concat( methods ? getSectionProtos( methods.childNodes ) : [] )
      .concat( methods2 ? getSectionProtos( methods2.childNodes ) : [] )
      .concat( genmethods ? getSectionProtos( genmethods.childNodes ) : [] );
}

/*---------------------------------------------------------------------*/
/*    Index generator                                                  */
/*---------------------------------------------------------------------*/
var arrs = stdlib.map( getChapterBindings );

console.log( JSON.stringify( [].concat.apply( [], arrs ) ) );
			  
