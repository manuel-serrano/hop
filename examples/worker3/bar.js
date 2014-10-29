var counter = 0;

function html( title ) {
   var count = <SPAN> { id: "counter", counter };
   
   return <HTML> {
      <H1> {
	 title
      },
      <DIV> {
	 "counter=", count,
      },
      <BUTTON> {
	 onclick: ~{
	    ${service () { return ++counter }}()
	       .post( function( v ) { ${count}.innerHTML = v } )
	 },
	 "inc me"
      }
   }
}

exports.html = html;
