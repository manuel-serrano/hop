/* ======================================================================
 *    HOP tabslider implementation
 * 
 *              Author: Erick Gallesio [eg@essi.fr]
 *       Creation date: 14-Sep-2005 09:24 (eg)
 *    Last file update: 16-Sep-2005 16:17 (eg)
 * ======================================================================
 */

/*---------------------------------------------------------------------*/
/*    hop_tabslider_select ...                                         */
/*---------------------------------------------------------------------*/
function hop_tabslider_select(item)
{
    var parent       = item.parentNode;
    var totalHeight  = parent.offsetHeight;
    var titlesHeight = 0;
    var selected;
    var i;

    for(i = 0; i < parent.childNodes.length; i+=2) {
	var title   = parent.childNodes[i];
	var content = parent.childNodes[i+1];

	titlesHeight += title.offsetHeight;
	if (title == item) {
	    content.style.display = "block";
	    selected = content;
	    title.className = "hop-tabslider-head-active";
	} else {
	    content.style.display = "none";
	    title.className = "hop-tabslider-head-inactive";
	}
    }
    /* Set the height of the selected item */
    selected.style.height = totalHeight - titlesHeight;
}


/*---------------------------------------------------------------------*/
/*    hop_tabslider_init ...                                           */
/*---------------------------------------------------------------------*/
function hop_tabslider_init(id)
{
    var ts = document.getElementById(id);
    hop_tabslider_select(ts.childNodes[0]);
}


/*---------------------------------------------------------------------*/
/*    Plug the tabsliders behaviour ...                                */
/*---------------------------------------------------------------------*/
hopBehaviour.register('hop-tabslider',
		      function (el) {
			  window.addEventListener(
			      'load', 
			      function(event) {
				  hop_tabslider_select(el.childNodes[0]);
			      },
			      false);
		      });

hopBehaviour.register('hop-tabslider-head',
		      function (el) {
			  el.onclick = function(){
			      hop_tabslider_select(this)
			  }
		      });
