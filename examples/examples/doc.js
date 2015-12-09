/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/examples/doc.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 19 10:32:06 2014                          */
/*    Last change :  Thu Nov 26 15:55:12 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Read and fontify the examples source codes.                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g examples.js                                       */
/*    browser: http://localhost:8080/hop/examples                      */
/*=====================================================================*/
var fs = require( "fs" );
var path = require( "path" );
var doc = require( "hopdoc" );
var markdown = require( hop.markdown );

/*---------------------------------------------------------------------*/
/*    examplesDoc ...                                                  */
/*---------------------------------------------------------------------*/
service examplesDoc( o ) {
   if( fs.existsSync( o.doc ) ) {
      try {
	 return <div>${ doc.load( o.doc ).XML }</div>;
      } catch( e ) {
	 console.error( "err=", e );
	 return <span/>;
      }
   } else {
      return <div>${ markdown.eval( o.doc ).XML }</div>;
   }
}

