/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/hopscript/spawn.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct  6 07:39:47 2016                          */
/*    Last change :  Wed Jan 17 07:38:54 2018 (serrano)                */
/*    Copyright   :  2016-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    WARNING: This file is not used to buid the hopscript library.    */
/*    It just illustrates the implementation of spawn.scm.             */
/*    -------------------------------------------------------------    */
/*    Spawn implementation as defined in                               */
/*      https://tc39.github.io/ecmascript-asyncawait                   */
/*      (See section "Informative desugaring")                         */
/*=====================================================================*/
"use strict";

/*---------------------------------------------------------------------*/
/*    promise                                                          */
/*---------------------------------------------------------------------*/
const promise = Promise;

/*---------------------------------------------------------------------*/
/*    spawn ...                                                        */
/*    -------------------------------------------------------------    */
/*    See generator.scm for the initialization.                        */
/*---------------------------------------------------------------------*/
function spawn( genF /* ::object */, self /* ::object */ ) {
   return new promise( function( resolve, reject ) {
      var gen = genF.call( self );
      function step( nextF ) {
         var next;
         try {
            next = nextF();
         } catch( e ) {
            // finished with failure, reject the promise
            reject( e );
            return;
         }
         if( next.done ) {
            // finished with success, resolve the promise
            resolve( next.value );
            return;
         }
         // not finished, chain off the yielded promise and `step` again
         Promise.resolve( next.value ).then( function( v ) {
            step( function() { return gen.next(v); } );
         }, function( e ) {
            step( function() { return gen.throw( e ); } );
         });
      }
      step( function() { return gen.next( undefined ); } );
   });
}

module.exports = spawn;
