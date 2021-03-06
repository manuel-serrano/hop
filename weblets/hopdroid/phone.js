/*=====================================================================*/
/*    serrano/prgm/project/hop/work/hopdroid/phone.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 26 16:42:35 2020                          */
/*    Last change :  Thu Nov 26 16:43:12 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    HopDroid phone connection                                        */
/*=====================================================================*/
"use hopscript"

import { hopdroid } from hop.hopdroid;

/*---------------------------------------------------------------------*/
/*    phone binding                                                    */
/*---------------------------------------------------------------------*/
export const phone = new hopdroid.phone();

/*---------------------------------------------------------------------*/
/*    event listener                                                   */
/*---------------------------------------------------------------------*/
phone.addEventListener( "configurationchanged", e => console.log( "config: ", e ) );
