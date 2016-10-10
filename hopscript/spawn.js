/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/hopscript/spawn.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct  6 07:39:47 2016                          */
/*    Last change :  Fri Oct  7 10:30:26 2016 (serrano)                */
/*    Copyright   :  2016 Manuel Serrano                               */
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
/*    See generator.scm for the initiailization.                       */
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
