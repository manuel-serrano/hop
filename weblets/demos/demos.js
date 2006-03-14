/*=====================================================================*/
/*    serrano/prgm/project/hop/weblets/demos/demos.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio                                    */
/*    Creation    :  Thu Mar  2 11:12:55 2006                          */
/*    Last change :  Mon Feb 14 06:14:00 2005                          */
/*    Copyright   :  2006 Erick Gallesio                               */
/*    -------------------------------------------------------------    */
/*    HOP Demos                                                        */
/*=====================================================================*/

function hop_switch_div(id) {
    var el1 = document.getElementById(id + "-1"),
	el2 = document.getElementById(id + "-2");
    
    if (el2.style.display == "none") {
	el1.style.display = "none";
	el2.style.display = "block";
    } else {
	el1.style.display = "block";
	el2.style.display = "none";
    }
}
