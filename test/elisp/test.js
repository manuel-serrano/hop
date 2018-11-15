/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/elisp/test.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Wed Nov 14 19:12:45 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    hopjs-mode indent tests                                          */
/*=====================================================================*/
"use hiphop";
"use strict";

// (let ((path (concat (getenv "PWD") "/../../etc"))) (setq debug-on-error t) (setq load-path (cons path load-path)) (load-library (concat path "/hopjs.el")) (load-library (concat path "/hopjs-parse.el")) (load-library (concat path "/hopjs-indent.el")) (hopjs-mode-hook))

// pok
{
   "servers": [ 
{
   "port": 587,
   "host": "smtp.gmail.com",
   "requireTLS": true,
   "authMethod": "LOGIN",
   "login": {
	  "user": "YYYY",
	  "pass": "XXX"
       }
},
	      {
		 "port": 587,
		 "host": "mail2-relais-roc.national.inria.fr",
		 "requireTLS": true,
		 "authMethod": "LOGIN",
		 "%login": {
			 "user": "YYYY",
			 "pass": "XXXX"
		      }
	      } ],
   
   
   "name": "redrock.inria.fr",
   
   "outOfMail": {
	      "hours": [ [20, 24] ],
	      "days": [ "sat", "sun" ]
	   },
   
   "immediateDelivery": [
"XXXX",
"YYYY",
"AAAA", "BBBBB"
]
}

// pok
async function main() {
   	 const argv = process.argv.slice( hop.standalone ? 1 : 2 );
   	 const minimist = require( 'minimist' );
   	 const args = minimist( argv, { names: ["-oi", "-bp", "-oQ", "-os"] });
      }
      
// pok
function foo() {
   if( args.g === true ) {
      dbg = {
	 fd: fs.openSync( config.log || "/tmp/hopsmtp.log", "a" ),
	 date: new Date()
      }
      syslog = {
	 log: function( ...args ) { debug.apply( undefined, args ) },
LOG_INFO: "info:",
LOG_ERROR: "error:",
open: function( path, mode ) { }
      }
   }
}

// pok
function openSMTPConnection( config ) {
   
   function open( server ) {
      syslog.log( syslog.LOG_INFO, "Creating "
+ ((server.secure || server.requireTLS) ? "SSL" : "")
	       + " connection to " + server.host );
      debug( "connecting to " + server.host );
   return new Promise( function( resolve, reject ) {
      const conn = new SMTPConnection( server );
      conn.on( 'error', reject );
      conn.connect( v => conn.login( server.login, () => resolve( conn ) ) );
   } );
}
   
   function loop( resolve, reject, i ) {
      debug( "in loop i=", i, " len=", config.servers.length );
      if( i >= config.servers.length ) {
	 reject( "no server available!" );
      } else {
		const server = config.servers[ i ];
		debug( "trying server: ", 
		       config.servers[ i ].host + ":" + config.servers[ i ].port );
	 	return open( server )
	       	      .then( conn => { 
				debug( "connection succeeded: ", server );
				conn.config = server; resolve( conn ) 
			     },
err => {
   debug( "connection failed: ", server );
   loop( resolve, reject, i + 1 );
} )
   }
   }
   
   return new Promise( (resolve, reject) => loop( resolve, reject, 0 ) );
}

// ok 
function patternHuman( request, approval, escalate ) {
   return hiphop module() {
      signal req;
      
      fork {
	 run request( req );
      } par {
	 every( req.now ) {
      	 }
      }
   }
}


// ok
function patternFanoutFanin( f1, f2s, f3 ) {
   run( f1 );
   fork ${f2s};
   run f3();
}

// ok
function patternChain( fs ) {
   return hiphop module() {
      signal exn;
	 
      abort( exn.now ) {
	 ${fs.map( m => hiphop run m( exn ) ) }
      }
      if( exn.nw ) { 
	 // error handling/compension goes here
      }
   }
}

// ok
hiphop module A( x, y ) {
   signal x;
   
   abort( exn.now ) {
      run m1();
      run m2();
   }
}

// ok
function foo() {
   async {
      return 1;
   }
}
	     
// ok
function foo() {
   var a = 1,
      b=2;
   return a+b;
}

// ok
function() {
   var a = 1;
   var b = 2;
   {
      return a+b;
   }
}

// ok 
function() {
   "foo"; "bar";
   var x = 3;
}

// ok
exports.dummySignalContainer = {};
a = 3;

// ok
enableLogin = __hh_module.MODULE(
   { a: 1 },
   {"id":"enableLogin","%location":{"filename":"login.hh.js","pos":149},"%tag":"module"},
   __hh_module.SIGNAL({"%location":{"filename":"login.hh.js","pos":169},"direction":"IN","name":"name"}),
   __hh_module.SIGNAL({"%location":{"filename":"login.hh.js","pos":178},"direction":"IN","name":"passwd"}),
   __hh_module.SIGNAL({"%location":{"filename":"login.hh.js","pos":196},"direction":"OUT","name":"loginEnabled"}),
   __hh_module.LOOPEACH({"%location":{"filename":"login.hh.js","pos":220},
			 "%tag":"do/every",
			 "immediate":false,
			 "apply":function (){
			    var name;
			    var passwd;
			    var hhaxs8671;
			    var hhaxs8672;
			 }
			})
   );

// ok
function HopcAstWalker( obj ) {
   if( obj instanceof Object ) {
      for( var f in obj ) {
	 this.f = obj[ f ];
      }
   }
}

// ok
const k = { 
   a: 3,
   b: 4 + 1,
   c: "toto",
   d: { 
      a: 10,
      b: [ 1, 2, 3 ],
      c: foo( 10 ),
      e: [ foo( 10 ),
	   bar( 30 ) ]
   },
   glop: function glop( x ) {
      if( #:isa?( ast, #:J2SNode ) ) {
	 return x;
      }
   }
};

// ok
let u = [   1, 2, 3, 4,
	    5, 6,
	    7, 9 ];

// ok
function foo( a ) {
   a ?
      1 : 2;
}

// ok
function foo( a ) {
   a 
      ? 1 : 2;
}

// ok 
function foo() {
   // toto n'est pas content
   x = 3;
}

// ok
function foo( a, b ) {
   return foo(
      bar( a ) );
}
      
// ok
function foo( a, b ) {
   return new hh.ctor(
      new hh.ctor2( a, b ) );
}

// ok
function foo( a, b ) {
   return 
      new hh.ctor(
      	 new hh.ctor2( a, b ) );
}

// ok
service helloServerDate() {
   return Date.now();
}

// ok
service bar() {
   var ZZZ = 
      56; var YYY = 3;
}

// ok
service bar( x, yyy ) {
   return xxxx + 
      yyy;
}

// ok
function bar() {
   function hux( a ) {
      return
	 a + 1;
   }
   function gee( x, yyy ) {
      return xxx + yyy;
      5;
   }
   return gee( 1, 2 );
}

// ok
function gee( a, b, c ) {
   function hux( a ) {
      return hux( a ) +
	 3;
   }
   if( a > b &&
       a < c ||
       3 ) {
      if( c > d ) 
	 aaa = 3;
      c = 0;
   }
}

// ok
function glop( x ) {
   return x  
      + y;
}

// ok
function glop( x ) {
   let x = glop( 10 )
      	 .post( function( snow ) {
		   return 23;
	     	} );
}

function glop( x ) {
   let x = glop( 10 )()
      	 .post( function( snow ) {
		   return 23;
	     	} );
}

// ok
function glop( x ) {
   let x = glop( 10 )
      	 .post( snow => {
		   return 32;
	     	} )
}

// ok
function glop( x ) {
   let x = glop( 10 )
      	 .post( (a, b) => {
		   return 32;
	     	} )
}

// ok
function glop( x ) {
   switch( obj.x.z ) {
      case 1:
      case 2: return 3;
      case 4:
	 return 5;
      default: return 6;
   }
}

// ok
service hello() {
   /* toto */var y = 3; var x = FOO(   x, y, z,
				       4, 5,
				       4 );
   try {
      foo( 1, 2, 
	   3, bar( x,
		   y ) );
      var x = <div>
	<span>
	  toto
	</span>
	<span>
	  <span>
	    <div>
	    </div>
	  </span>
	</span>
      </div>;
      var x = <div>
	<span>
	  <div>
	    toto
	  </div>
	  tutu
	</span>
      </div>;
      
      if( x > 3 ) {
	 x = 3;
	 y = 6;
	 z = 5 + 6;
	 x++;
      } else {
	 x = 5;
	 y--;
	 x += 1;
      }
   }
}

// ok
function test() {
   var sdate = <TD id="sdate">${new Date( Date.now() )}</TD>;
   var cdate = <TD id="cdate">-</TD>;
   
   return <table>
     <tr>
       ${date}
     </tr>
   </table>
   x = 3;
}

// ok
function test() {
   return <html>
     <div x=3>
       <span>
       </span>
     </div>
   </html>
}

// ok
function test() {
   return <html y=3>
     <div x=3>
       <span>
       </span>
     </div>
   </html>
}

// ok
function test() {
   var sdate = <TD id="sdate">${new Date( Date.now() )}</TD>;
   var cdate = <TD id="cdate">-</TD>;
   return <HTML>
     <BUTTON onclick=~{
		x++;
	     }
	     z=${1+2}>
       <div>
	 <span>
	   ceci est un span
on va voir ce quon va voir
cest juste du texte...
	 </span>
	 <div>
	   <span>
	     <table>
	     </table>
	   </span>
	 </div>
       </div>
     </BUTTON>
   </HTML>
}

// ok
service glop() {
   return <DIV>
     <table>
     </table>
   </DIV>
}

// ok
service glop() {
   return <div class="foo"
	       id="bar">
}

// ok
service glop() {
   return <div onclick=~{
		  alert( "foo" )
	       }>
     yip
   </div>
}

// ok
service glop() {
   return <div onclick=~{
		  alert( "foo" )
	       }>
     <span>
       toto
     </span>
   </div>
}

// ok
service glop() {
   return <DIV onclick=~{
		  ${helloServerDate}()
		     .post( function( snow ) {
			       ${sdate}.innerHTML = 
				  new Date( snow ).toString();
			       ${cdate}.innerHTML = 
				  new Date( Date.now() ).toString();
			    } )
	       }>
     Click me to update dates...
     <table>
       <TR><TH>server date: ${sdate}</TH></TR>
       <TR><TH>client date: ${cdate}</TH></TR>
     </TABLE>
   </DIV>;
}

// ok
function foo() {
   let a = 1, b = 2, c = 3;
   return a + b + c;
}

// ok
const hhparser = function( token ) {
   const loc = token.location;
   let pre = false, val = false, access = "present";

   //this.consumeToken( this.LPAREN );
   this.consumeToken( this.DOT );
}

// ok
service main() {
   return <html>
     <head>
       <script src="hiphop" lang="hiphop"/>
       <script src="./login.hh.js" lang="hiphop"/>
       <script defer>
	 const login = require( "./login.hh.js" );
       </script>
     </head>
}

