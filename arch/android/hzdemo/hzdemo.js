/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/arch/android/hzdemo/hzdemo.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Fri Nov 13 15:03:23 2020                          */
/*    Last change :  Fri Nov 13 15:03:53 2020 (serrano)                */
/*    Copyright   :  2020 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    A simple Android Hz application.                                 */
/*=====================================================================*/
"use hopscript"

/*---------------------------------------------------------------------*/
/*    module                                                           */
/*---------------------------------------------------------------------*/
const hopdroid = require( hop.hopdroid );

/*---------------------------------------------------------------------*/
/*    Connection with the Android API                                  */
/*---------------------------------------------------------------------*/
const phone = new hopdroid.phone();

/*---------------------------------------------------------------------*/
/*    The application                                                  */
/*---------------------------------------------------------------------*/
service hzdemo() {
   return <html>
     <h1>Phone: ${phone.model}</h1>
     <h2>Contact:</h2>
     <ul>
       ${phone.contact().map( e => <li>${e.familyname} ${e.firstname}: ${e.phones.default} ${e.emails} </li> )}
     </ul>
   </html>
}

/*---------------------------------------------------------------------*/
/*    Contact list                                                     */
/*---------------------------------------------------------------------*/
service contact() {
   return JSON.stringify( phone.contact() );
}

contact.path = "/hop/hzdemo/contact";
