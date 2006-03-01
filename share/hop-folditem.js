/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-folditem.js                   */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio                                    */
/*    Creation    :  Wed Mar  1 11:56:02 2006                          */
/*    Last change :  Wed Mar  1 11:58:19 2006 (eg)                     */
/*    -------------------------------------------------------------    */
/*    HOP fold-item implemenetation                                    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*     hop_fold_item_icon_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_fold_item_icon_set(id, img)
{
    var ul = document.getElementById(id).parentNode.parentNode; 
    
    ul.style.setProperty("list-style-image", "url(" + img + ")", "");
}

/*---------------------------------------------------------------------*/
/*     hop_fold_item_toggle ...                                        */
/*---------------------------------------------------------------------*/
function hop_fold_item_toggle(id, open, close)
{
    var el = document.getElementById(id);
    var ul = el.parentNode.parentNode;
    var tmp1, tmp2;
    
    if (el.style.display == "block") {
	tmp1 = "none";
	tmp2 = close;
    } else {
	tmp1 = "block";
	tmp2 = open;
    }

    el.style.display = tmp1;
    hop_fold_item_icon_set(id, tmp2);
}
