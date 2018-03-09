/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/examples/lang/csv.js              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Mar  9 08:41:47 2018                          */
/*    Last change :  Fri Mar  9 10:05:04 2018 (serrano)                */
/*    Copyright   :  2018 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    A cvs loader                                                     */
/*=====================================================================*/



"use hopscript"

const csvloader = require( "./csv.hop" );

exports[ Symbol.compiler ] = file => {
   return {
      type: "value",
      value: csvloader.load( file )
   }
};
