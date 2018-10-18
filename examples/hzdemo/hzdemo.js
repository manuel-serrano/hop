/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/hzdemo/hzdemo.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:10 2014                          */
/*    Last change :  Sat Dec 20 08:02:38 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Programmable hophz interactions                                  */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g hzdemo.js                                         */
/*    browser: http://localhost:8080/hop/hzdemo                        */
/*=====================================================================*/
var HOP = require( "hop" );
var HOPHZ = require( "hophz" );

service hzdemo() {
   return <HTML> {
      ~{
	 function install( url ) {
	    ${hophzDownload}( url )
	       .post( function( e ) { alert( "weblet installed..." ); },
		      function( e ) { alert( "error status=" + e ); } );
	 }
      },
      <DIV> {
	 "install: ",
	 <INPUT> {
	    type: "text",
	    id: "url",
	    value: "http://localhost:9999" + hzdemo.resource( "foo-1.0.0.hz" )
	 },
	 <BUTTON> {
	    onclick: ~{
	       var el = document.getElementById( "url" );
	       install( el.value );
	    },
	    "install"
	 }
      },
      <DIV> {
	 "search: ",
	 <INPUT> { type: "text", id: "search" },
	 <BUTTON> {
	    onclick: ~{
	       var el = document.getElementById( "search" );
	       ${hophzSearch}( el.value )
		  .post( function( e ) {
		     var el = document.getElementById( "console" );
		     el.innerHTML = "";

		     if( e ) {
			e.forEach( function( e ) {
			   el.appendChild( <DIV> {
			      e.name, " ", e.version,
			      <BUTTON> {
				 onclick: ~{ install( ${e.url} ) },
				 "install"
			      }
			   } )
			} )
		     } else {
			el.innerHTML = "not found";
		     }
		  } )
	    },
	    "search"
	 }
      },  
      <DIV> {
	 "find: ",
	 <INPUT> { type: "text", id: "find" },
	 <BUTTON> {
	    onclick: ~{
	       var el = document.getElementById( "find" );
	       ${hophzFind}( el.value )
		  .post( function( e ) {
		     var el = document.getElementById( "console" );
		     el.innerHTML = "";

		     if( e ) {
			el.appendChild( <DIV> { e.name, " ", e.version, " ", e.author } )
			el.appendChild( <DIV> { e.comment } );
		     } else {
			el.appendChild( <DIV> { "weblet not found" } ) 
		     }
		  } );
	    },
	    "find"
	 }
      },
      <DIV> { id: "console" },
      <H1> { "categories" },
      <UL> {
	 HOPHZ.listCategories( "localhost" ).map( function( e ) {
	    return <LI> {
	       <A> { href: hophzCategory( e ).toString(), e }
	    }
	 } )
      }
   }
}

service hophzCategory( c ) {
   return <HTML> {
      <H1> { c },
      <UL> {
	 HOPHZ.listWeblets( { category: c }, "localhost" ).map( function( e ) {
	    return <LI> {
	       <A> {
		  e.name, " ",
		  <BUTTON> {
		     onclick: ~{ install( e.url ); },
		     "install"
		  }
	       }
	    }
	 } )
      }
   }
}

service hophzDownload( url ) {
   try {
      return HOPHZ.download( url, "localhost" );
   } catch( e ) {
      return HOP.HTTPResponseString( e.msg, { startLine: "HTTP/1.0 404 File not found" } );
   }
}

service hophzSearch( regexp ) {
   return HOPHZ.search( regexp, "localhost" );
}

service hophzFind( name ) {
   return HOPHZ.find( name, "localhost" );
}

console.log( "Go to \"http://%s:%d/hop/hzdemo\"", HOP.hostname, HOP.port );
