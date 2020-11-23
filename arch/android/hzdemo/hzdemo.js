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
   let el = <input/>;

   return <html>
     <script>
       function removeContact( id ) {
	  ${service(cid) { phone.removeContact( cid ) }}( id ).post();
       }
       function setStatusBarColor( col ) {
	  ${service(c) { phone.statusBarColor = c; } }( col ).post();
       }
     </script>
     <h1>Phone: ${phone.model}</h1>
     
     <h1>Ui:</h1>
     Set status bar color 
     <button onclick=~{setStatusBarColor( "#a00" )}>red</button>
     <button onclick=~{setStatusBarColor( "#0a0" )}>green</button>
     <button onclick=~{setStatusBarColor( "#00a" )}>blue</button>
	       
     <h1>Contact:</h1>
     <ul id="contacts">
       ${phone.contacts().map( e => 
	       <li id=${e.id}>
		 ${e.id}:
                 <button onclick=~{
			    let cid = ${e.id};
			    let ul= document.getElementById( "contacts" );
			    let li = document.getElementById( cid );
			    removeContact( cid ); 
			    ul.removeChild( li );
			 }>
		   remove
		 </button>
                 ${e.familyname} ${e.firstname}: ${e.phones.default} ${e.emails} 
	       </li> )}
     </ul>
     <div>
       ${el}
       <button onclick=~{let v = ${el}.value; if( v !== "" ) { removeContact( v ); }}>
	 remove
       </button>
     </div>
   </html>
}

/*---------------------------------------------------------------------*/
/*    Contact list                                                     */
/*---------------------------------------------------------------------*/
service contact() {
   return JSON.stringify( phone.contacts() );
}

contact.path = "/hop/hzdemo/contact";

