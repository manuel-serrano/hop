/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/etc/mkthissymbols.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Jan  8 10:49:22 2021                          */
/*    Last change :  Thu Dec  9 14:02:42 2021 (serrano)                */
/*    Copyright   :  2021 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    mkthissymbols                                                    */
/*    -------------------------------------------------------------    */
/*    This is used to generate the js2scheme/this-symbols.sch file.    */
/*    It contains the list of pre-defined THIS properties that         */
/*    hopc reads in order to avoid irrelevant "unbound variable"       */
/*    hopscript mode error messages.                                   */
/*=====================================================================*/
"use hopscript";

console.log( ";; Generated file (mkthissymbols.js). Don't edit" );

console.log( "(hop require module" );
Object.getOwnPropertyNames( this )
   .sort( (x, y) => x <= y )
   .forEach( k => console.log( k, " " ) );
console.log( ")" );

