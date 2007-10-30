/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-fx.js                         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May  4 16:34:59 2006                          */
/*    Last change :  Tue Oct 30 06:47:28 2007 (serrano)                */
/*    Copyright   :  2006-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP graphical effects library                                    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_fx_repeat ...                                                */
/*---------------------------------------------------------------------*/
function hop_fx_repeat( proc, timeout ) {
   var it = false;
   var p = function() { if( !proc() && it ) clearInterval( it ) };
   it = setInterval( p, timeout );
}

/*---------------------------------------------------------------------*/
/*    hop_fx_fade_background ...                                       */
/*---------------------------------------------------------------------*/
function hop_fx_fade_background( obj, delay, step, colors ) {
   var it;
   var i = 0;
   var func = function() {
      obj.style.background = colors[ i ];
      i++;

      return (i < colors.length);
   }

   obj.style.background = colors[ i ];
   it = setInterval( function() {
                       clearInterval( it );
                       hop_fx_repeat( func, step )
                     }, delay );
}

/*---------------------------------------------------------------------*/
/*    hop_fx_make_shadow ...                                           */
/*---------------------------------------------------------------------*/
function hop_fx_make_shadow( cla, obj ) {
   return "\n\
<table class='" + cla + "' cellspacing='0' cellpadding='0' border='0'>\n\
  <tr>\n\
    <td class='hop-window-shadow-nw' rowspan='2' colspan='2'>\n" +
   (((obj instanceof String) || (typeof obj == "string")) ? obj : obj.innerHTML) +
   "</td>\n\
    <td class='hop-shadow-ne'>&#160;</td>\n\
  </tr>\n\
  <tr>\n\
    <td class='hop-shadow-e'>&#160;</td>\n\
  </tr>\n\
  <tr>\n\
    <td class='hop-shadow-sw'>&#160;</td>\n\
    <td class='hop-shadow-s'>&#160;</td>\n\
    <td class='hop-shadow-se'>&#160;</td>\n\
  </tr>\n\
</table>";
}
