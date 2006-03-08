/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-foldlist.js                   */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio                                    */
/*    Creation    :  Wed Mar  1 11:56:02 2006                          */
/*    Last change :  Tue Mar  7 17:50:28 2006 (eg)                     */
/*    -------------------------------------------------------------    */
/*    HOP fold-item implemenetation                                    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*     hop_fold_item_toggle ...                                        */
/*---------------------------------------------------------------------*/
function hop_fold_item_toggle(id, open, close)
{
    var el  = document.getElementById(id);
    var img = document.getElementById(id + "-img");
    
    if (el.style.display == "block") {
	el.style.display = "none";
	img.src = close;
    } else {
	el.style.display = "block";
	img.src = open;
    }
}