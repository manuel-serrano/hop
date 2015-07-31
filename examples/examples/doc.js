/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/examples/doc.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 19 10:32:06 2014                          */
/*    Last change :  Thu Jul 30 17:07:20 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Read and fontify the examples source codes.                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g examples.js                                       */
/*    browser: http://localhost:8080/hop/examples                      */
/*=====================================================================*/
var hop = require( "hop" );
var fs = require( "fs" );
var path = require( "path" );
var doc = require( "hopdoc" );

/*---------------------------------------------------------------------*/
/*    examplesDoc ...                                                  */
/*---------------------------------------------------------------------*/
service examplesDoc( o ) {
   if( fs.existsSync( o.doc ) ) {
      try {
	 return <DIV>${doc.parseFile( o.doc ).XML}</DIV>;
      } catch( e ) {
	 console.error( "err=", e );
	 return "";
      }
   } else {
      return "";
   }
}

