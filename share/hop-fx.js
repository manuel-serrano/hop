/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-fx.js                         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May  4 16:34:59 2006                          */
/*    Last change :  Thu May  4 17:05:18 2006 (serrano)                */
/*    Copyright   :  2006 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    HOP graphical effects library                                    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_fx_repeat ...                                                */
/*---------------------------------------------------------------------*/
function hop_fx_repeat( proc, timeout ) {
   var it;
   var p = function() { if( !proc() ) clearInterval( it ) };
   it = setInterval( p, timeout );
}

/*---------------------------------------------------------------------*/
/*    hop_fx_fade_background ...                                       */
/*---------------------------------------------------------------------*/
function hop_fx_fade_background( obj, delay, step, colors ) {
   var it;
   var i = 0;
   var func = function() {
      i++;
      obj.style.background = colors[ i ];
      return (i < colors.length);
   }

   obj.style.background = colors[ i ];
   it = setInterval( function() {
                       clearInterval( it );
                       hop_fx_repeat( func, step )
                     }, delay );
}
