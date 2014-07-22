exports.hello = function( s ) {
   return <DIV> {
      onclick: ~{
	 alert( "s=" + s );
      },
      s
   }
}
