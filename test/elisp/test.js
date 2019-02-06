/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/elisp/test.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Tue Feb  5 15:39:42 2019 (serrano)                */
/*    Copyright   :  2014-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    hopjs-mode indent tests                                          */
/*=====================================================================*/
"use hiphop";
"use strict";

/*
(let ((path (concat (getenv "PWD") "/../../etc")))
  (setq debug-on-error t)
  (setq load-path (cons path load-path))
  (load-library (concat path "/hopjs.el"))
  (load-library (concat path "/hopjs-parse.el"))
  (load-library (concat path "/hopjs-indent.el"))
  (hopjs-mode-hook))

[\C-x\C-t] for testing 
or
ESC-x: hopjs-indent-test
or
(hopjs-indent-test)

[\C-x\C-z] for reloading the emacs file 
*/

/*---------------------------------------------------------------------*/
/*    JavaScript                                                       */
/*---------------------------------------------------------------------*/
function react( o ) {
   if( "name" in o ) nameKeypress( o.name );
   else throw `Illegal signal ${o}`;
	
	return 3;
}

// comment
function foo() {
/* a comment */
   return 3;
}

// ok conditional
function startSession() {
   if( intvreg ) clearInterval( intvreg );
   conntime.nowval = 0;
}

// ok comment and conditionals
function foo() {
   for( let i = 0; i < len; i++ ) {
      const fanout = fanin_list[ i ];
      let value = fanout.polarity ? thus.value : !this.value;
      return value;
   }
}

// ok const
const K = 10;
const N = K / 10;

function foo() {
   return undefined;
}

// pok closing (because of ./struct
const js = <impress.slide title="Intro JavaScript"
			  id="intro-js"
			  data-y=0
			  class="intro"
			  data-step-count=3>
  <div id="scm-access" class="scm code">
    <tt class="filename">./struct.scm</tt>
    ${code.include( require.resolve( "./src/class.scm" ), fontifier.scheme )}
  </div>
</impress.slide>  

// ok arrow									 
json.forEach( entry => { 
      console.log( entry );
   } );

// ok !new
function bangnew() {
   if( true ) {
      return !new X()
	 && 3;
   } else {
      return new TypedArray([1, 2, 3]).includes(1)
      	 && !new TypedArray([1, 2, 3]).includes(4)
	 && !new TypedArray([1, 2, 3]).includes(1, 1);
   }
}

// ok, ! in expr
function includesa() {
   return ![1, 2, 3].includes(1)
      && ![1, 2, 3].includes(4)
      && ![1, 2, 3].includes(1, 1);
}

// ok, ! in expr
function includesa() {
   return !x
      && !y
      && !z;
}

// ok, nested switches
function s() {
   while( true ) {
      switch( this.peekToken().type ) {
	 case this.RPAREN:
	    thas.consumeAny();
	    const attrs = astutils.J2SObjInit( loc, inits );
	    return astutils.J2SCall( loc, hhref( loc, "RUN" ), null,
	       [ attrs ].concat( accessors ) );
	    
	 case this.ID:
	    const a = this.consumeAny();
	    
	    switch( this.peekToken().type ) {
	       case this.COMMA:
		  this.consumeAny();
		  inits.push( astutils.J2SDataPropertyInit(
		  		 a.loc, astutils.J2SString( a.loc, a.id ),
		  		 astutils.J2SString( a.loc, "" ) ) );
		  break;
		  
	       case this.EGAL:
		  this.consumeAny();
		  const { expr, acessor } = this.parseHHExpression();
		  break;
		  
	       default:
		  const as = this.consumeToken( this.ID );
	    }
	    break;
	    
	 case this.DOTS:
	    this.consumeAny();
	    inits.push( astutils.J2SDataPropertyInit(
			   a.loc, astutils.J2SString( a.loc, "autocomplete" ),
			   astutils.J2SBool( a.loc, true ) ) );     
	    break;
	    
	 default:
	    throw tokenTypeError( this.consumeAny() );
      }
   }
}

// ok, argument alignment
function align() {
   const assigframe =
      astutils.J2SStmtExpr( 
      	 loc, 
      	 astutils.J2SAssig( 
	    loc, hhref( loc, "runFrame" ), 
	    astutils.J2SRef( loc, param ) ) );
}

// ok, top level module clauses
function topl() {
   export * from "./machine.js";
   
   return 3;
}

// ok, comment after opening brace
function F( a, 
   b ) { // this is a comment
   return 3;
}

// ok, switch default
function parse() {
   switch( this.peekToken() ) {
      case this.ID:
	 return 1;
      case this.DOLLAR:
	 return 2;
      default:
   	 throw tokenTypeError( this.consumeAny() );
   }
}

// ok, do/while
function parseInterfaceIntflist( x, y ) {
   do {
      x++;
   } while( x < 10 )
   
   return y;
}

// ok, && after array assignment
function kangaxr() {
   return RegExp.prototype.exec.call(obj, "foobarbaz")[0] === "baz"
      && obj.lastIndex === 9 && obj instanceof F;
}

// ok, empty lines and tabs
function mdnn() {
   const object1 = {};
   if( Reflect.isExtensible(object1) !== true ) return false;
   
   
   Reflect.preventExtensions(object1);
}


// ok, expression after : ? xml
const Cart = ( { products, total, onCheckoutClicked }, ... ) => {
   const hasProducts = products.length > 0;
   const nodes = hasProducts 
      ? products.map( product => 
	 <Product title=${product.title}
		  price=${product.price}
		  quantity=${product.quantity}
		  key=${product.id}/> )
      : <em>cart</em>;
   
   return <div>3</div>;
}


// ok, colon after question mark
const Cart = ( { products, total, onCheckoutClicked }, ... ) => {
   const nodes = hasProducts 
      ? products.map( product => 0 )
      : <em>Please add some products to cart.</em>;
}

// ok , indent arrow variable bound function
const Cart = ( { products, total, onCheckoutClicked }, ... ) => {
   return 3;
}


// ok, else if sequence
function automata() {
   tick = tick + 1;
   console.log("Tick:", tick);
   if (tick === 5 ) {
      open("Percussion");
      percuState = true;
   } else if (tick > 5) {
      if (percuState && event("Percussion")) {
	 return 3;
      }
   }
   return 5;
}

// ok map over a literal array
service reflex ( f ) {
   [a, b, c]
      .map( f );
}


// ok return after in
function foo() {
   try {
      'foo' in proxy;
      return false;
   } catch(e) {}
}

// ok
function kangaxt() {
   function F(){}
   var p1 = Reflect.construct(Promise,[function(resolve, reject) { resolve("foo"); }], F);
   var p2 = Reflect.construct(Promise,[function(resolve, reject) { reject("quux"); }], F);
   var score = +(p1 instanceof F && p2 instanceof F);
   
   function thenFn(result)  { score += (result === "foo");  check(); }
   function catchFn(result) { score += (result === "quux"); check(); }
   function shouldNotRun(result)  { score = -Infinity;   }
   
   p1.then = p2.then = Promise.prototype.then;
   p1.catch = p2.catch = Promise.prototype.catch;
   
   p1.then(thenFn, shouldNotRun);
   p2.then(shouldNotRun, catchFn);
   p1.catch(shouldNotRun);
   p2.catch(catchFn);
   
   function check() {
      assert.ok( score === 4, "kangaxt" );
   }
   
   return true;
}

// ok after if then branch
function mdno() {
   var object1 = {};
   if( Reflect.setPrototypeOf(object1, Object.prototype) !=== true )
      return false;
   
   return object1.x;
}

// ok after passed assignment
function mdnk() {
   const object1 = {};
   var passed = Reflect.isExtensible(object1) === true;
   
   Reflect.preventExtensions(object1);
}

// ok after if line
function mdnk() {
   const object1 = {};
   if( Reflect.isExtensible(object1) !== true ) return false;
   
   Reflect.preventExtensions(object1);
}

// ok second "&&" indent
function mdnk() {
   const object1 = {
      property1: 42
   };
   
   return Reflect.getOwnPropertyDescriptor(object1, 'property1').value === 42
      && Reflect.getOwnPropertyDescriptor(object1, 'property2') === undefined
      && Reflect.getOwnPropertyDescriptor(object1, 'property1').writable === true;
}

// ok bad indent after in-line return
function( x ) {
   if( x > 20 ) return false;
   
   return true;
}

// ok: literal indent (after new Proxy)
function func( domain, range ) {
   return function( info ) {
      const ri = range(info);
      const di = domain( !info );
      return {
      	 go: function( value ) {
	    if( typeof value === "function" ) {
	       return new Proxy( value, { 
	       	  apply: function( target, self, args ) {
		     return ri.go( target( di.go( args[ 0 ] ) ) );
		  },
		  toString: function() {
		     return "<" + "value.toString()" + ">";
		  }
	       } )
      	    } else {
	       throw "Not a function info=" + info;
      	    }
      	 }
      }
   }
}

// ok: dot indentation  
function addNumber( G ) {
   G.mach
      .getElementById( "numbers" )
      .appendChild( number( G ) )
      .doit();
}

// return literal
exports[ Symbol.compiler ] = (file, options) => {
   const fd = fs.openSync( options.target, "r" );
   try {
      const val = fs.readFileSync( file, "utf8", function ( err, data ) {
	 if( !err ) {
	    return {
	       type: "value",
	       value: data.replace( /#[^#]*#/g, "" )
	    };
	 } else {
	    return {
	       type: "error",
	       value: err
	    }
	 }
      } );
   } finally {
      fs.closeSync( fd );
   }
}

// indenting arrow function after equal sign
exports[ Symbol.compiler ] = (file, options) => {
   const val = fs.readFileSync( "foo.txt" );
}

// bar function lbrace indent
ReactiveMachine.prototype.Promise =
   function( signame ) {
      return new Promise( function( resolve, reject ) {
      	 m.addEventListener( signame, function( evt ) {
	    resolve( evt.signalValue );
         } );
      } );
   };

// comma and binop
function() {
   if( mach.profile ) {
      const profend = Date.now();
      console.error( "reaction duration: " + (profend - profstart),
	 " comptime: " + (profcomp - profstart),
	 " propagation: "+ (profpropend - profpropstart),
	 " exec: " + (profpropend - profexecstart ));
   }
}      

// case
function() {
   switch(msgRecu.type) {
      
      case "abletonPseudo":
	 break;
 	 
      case "abletonReloadConfAgit":
	 ableton.initAbletonTable("./pieces/controleAbletonAgitV2.csv");
	 break;
	 
      case "abletonReloadConfMortDuGolem":
	 ableton.initAbletonTable("./pieces/controleAbletonMortDuGolemV2.csv");
	 break;
   }
}

// ok
function foo( a, b ) {
   #:tprint( a );
   tprint( b );
}

// ok
function chainAnimationsGenerator (elem, animations ) {
   try {
      x = 3;
   } catch( e ) { 
      return ret;
   }
}

// ok
function chainAnimationsGenerator (elem, animations ) {
   return spawn( function* () {
      let ret = null;
      try {
   	 for(const anim of animations) {
      	    ret = yield anim(elem);
   	 }
      } catch(e) { /* ignore and keep going */ }
      return ret;
   });
}

// ok
function* fibonacci() {
   let fn1 = 0, let fn2 = 1;
   
   while( true ) {  
      let current = fn1;
      fn1 = fn2;
      fn2 = current + fn1;
      let reset = yield current;
      if( reset ) {
	 fn1 = 0;
	 fn2 = 1;
      }
   }
}

// ok
function read( fd, chars ) {
   fs.read( fd, new Buffer( 10 ), 0, 10, null, 
      (err, readCount, data) => {
      	 if( readCount > 0 ) {
	    read( fd, chars + data );
      	 } else {
	    fs.close(fd);
	    console.log("File content : " + fileContent);
      	 }
      } )
}

// ok
class ActionArg {
   constructor( machine, accessor_list ) {
      this.machine = machine;
      
      accessor_list.forEach( acc => {
	    this[ acc.signame ] = {
	       preval: undefined,
	       nowval: undefined,
	       pre: undefined,
	       now: undefined
	    };
      	 } );
   }
}

// ok
function fill( accessor_list, lvl ) {
   accessor_list.forEach( 
      acc => {
	 let sig = acc.signal;
	 let min_lvl = lvl > sig.ast_node.depth ? sig.ast_node.depth : lvl;
      	 
      	 this.preValue[ acc.signame ] = acc.signal.pre_value;
      	 this.value[ acc.signame ] = acc.signal.value;
      	 this.prePresent[ acc.signame ] = acc.signal.pre_gate.value;
      	 this.present[ acc.signame ] = sig.gate_list[ min_lvl ].value;
      } );
}

// ok
function() {
   return raw_signals.map( function(el, i, arr ) {
      let signalName;
      let type;
      let s = el.split( "." );
      
      if( s[0] != "this" ) {
	 throw error.TypeError( "Wrong accessor parsing (1)", loc );
      } else if( s.length == 2 ) {
	 s = s[ 1 ].split( '["' );
	 signalName = s[ 0 ];
	 type = s[ 1 ].substr( 0, s[ 1 ].length - 2 );
      } else if( s.length == 3 ) {
	 signalName = s[ 1 ];
	 type = s[ 2 ];
      } else {
	 throw error.TypeError( "Wrong accessor parsing (2)", loc );
      }
      
      console.log( "sn=", signalName, "ty=", type );
      return makeAccessor( signalName, type, loc );
   });
}

// ok
function readFileAsync( fd ) {
   var buffer = new Buffer( 10 );
   buffer.fill( 0 );
   fs.read( fd, buffer, 0, 10, null, function (err, readCount, data) {
      console.log("Read %d bytes.", readCount);
      if( readCount > 0 ) {
	 fileContent += data.toString().substr(0, readCount);
  	 readFileAsync(fd);
      } else {
 	 fs.close(fd);
	 console.log("File content : " + fileContent);
      }
   })
}

// ok
function readFileAsync( fd ) {
   var buffer = new Buffer( 10 );
   buffer.fill( 0 );
   fs.read( fd, buffer, 0, 10, null, (err, readCount, data) => {
      console.log("Read %d bytes.", readCount);
      if( readCount > 0 ) {
	 fileContent += data.toString().substr(0, readCount);
	 readFileAsync(fd);
      }else {
	 fs.close(fd);
	 console.log("File content : " + fileContent);
      }
   })
}

// ok
modules.exports = {
   name: "lib",
   author: "me again"
}

// ok
function run( tmt, lbl ) {
   let last = Date.now();
   
   setInterval( () => { 
      	 let cur = Date.now();
      	 console.log( lbl ) ;
      },
      tmt );
}

// ok
function mkPromise( name, result, tmt ) {
   return new Promise( 
      (resolve, reject) => {
	 setTimeout( 
	    () => { 
	       console.log( "in", name );
	       if( result ) resolve( result ) else reject( false );
	       return 3;
	    } )
      } );
}

// ok
function mkPromise( name, result, tmt ) {
   return new Promise( 
      (resolve, reject) => {
	 setTimeout( 
	    () => { 
	       console.log( "in", name );
	       if( result ) 
		  resolve( result ) else reject( false );
		  return 3;
	    } )
      } )
}

// ok 
const p1 = new Promise( 
   (resolve, reject) => {
      return 3;
   } );

// ok
function promiseToModule( promise ) {
   return module( resolve, reject ) {
      async {
	 promise.then( v => this.react( { resolve: v } ),
	    v => this.react( { reject: v } ) );
      } kill {
	 if( "cancel" in promise ) {
	    promise.cancel();
	 }
      }
   }
}

// ok
function foo( x ) {
   return function bar( a, b ) {
      return 3;
   }
}

// ok
function foo( x ) {
   return function( a, b ) {
      return 3;
   }
}

// ok
function foo( x ) {
   return module( a, b ) {
      return 3;
   }
}

// ok
async function main() {
   const argv = process.argv.slice( hop.standalone ? 1 : 2 );
   const minimist = require( 'minimist' );
   const args = minimist( argv, { names: ["-oi", "-bp", "-oQ", "-os"] });
}

// ok
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

// ok
function openSMTPConnection( config ) {
   
   function open( server ) {
      syslog.log( foobar.LOG_INFO, 
	 "Creating "
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
      e: [  foo( 10 ),
	    bar( 30 ),
      	    gee( 20 ) ]
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

/*---------------------------------------------------------------------*/
/*    Plugins                                                          */
/*---------------------------------------------------------------------*/
service foo();
	
// ok
module.exports = hiphop machine( in A, in B, in R, out O ) {
   do {
      fork {
	 yield;
      	 emit A();
      }
   }
}

// ok run after lbrace and m.react after rbrace
function() {
   const m = hiphop machine() {
      run User();
   }
   
   m.react();
}

// ok: error on [Newline] after push( x );		   
hiphop machine mach( O ) {
   signal L = [] combine (x,y) => y.push( x );
}

// ok: bad indent after fork $		   
function forkLast( funs ) {
   function par( f ) {
      return hiphop async L { f( v => { this.notify( v ); } ) };
   }
   
   hiphop machine mach( O ) {
      signal L;
      
      fork ${ funs.map( par ) }
      	   if( L.now ) {
	      emit O( { resolve: true, val: L.nowval } );
      	   } else {
	      emit O( { resolve: false } );
      	   }
   }
   
   return new Promise( function( resolve, reject ) {
      mach.addEventListener( "O", v => {
	 v.nowval.resolve ? resolve( v.nowval.val ) : reject( undefined )
      } );
      mach.react();
   } );
}

// ok
function makeAutomatePossibleMachine () {
   function creationModule( lesSons ) {
      return hiphop module automateDesPossibles(in tick, in abletonON)
	 implements 
	    automateInt.creationInterfaces(lesSons[0]),
      	    automateInt.creationInterfaces(lesSons[1]),
      	    automateInt.creationInterfaces(lesSons[2]) {
   	       fork {
      	       	  //run spy(...);
   	       } par {
      	       	  every ( immediate abletonON.now) {
	 	     if ( nowval( abletonON) === 1) {
	    	     	run automateUn.trajetModule( ... );
	 	     } else if  ( nowval(abletonON) === 2){
			run automateDeux.trajetModule(...);
		     } else if ( nowval(abletonON) === 3){
			run automateTrois.trajetModule(...);
		     }
      	       	  }
   	       }
	    }
   }
   
   var  machine = new hh.ReactiveMachine( creationModule( par.groupesDesSons) );
   for (var j=0; j < par.groupesDesSons.length; j ++) {
      for (var i=0; i < par.groupesDesSons[j].length; i++) {
	 var signal = par.groupesDesSons[j][i][0] + "OUT";
	 
	 if (debug) console.log("Addeventlisterner:signal:",signal);
      }
   }
}

// ok
function number( G ) {
   let trap = hiphop {
      exit: loop {
	 await( killn.now && numbers.now );
	 emit numbers( num );
	 emit killn( prey( numbers.nowval ) );
	 
	 if( killn.nowval.indexOf( num ) >= 0 ) {
	    hop {
	       num.dead = true;
	       G.mach.getElementById( "numbers" ).removeChild( trap );
	    }
	    break exit;
	 } else {
	    if( num.prey && killn.nowval.indexOf( num.prey ) >= 0 ) {
	       hop { num.init() }
	    }
	    hop { num.move() }
	 }
      }
   }
   
   return trap;
}


// ok
function firstPromise( promises ) {
   return new Promise( 
      (resolve, reject) => {
	 hiphop machine m( resolve, reject ) {
	    fork {
               abort( resolve ) {
	       	  ${ promises.map( 
			p => hiphop { run (promiseToModule( acc ))( ... ) } ) }
      	       }
      	    }
      	 }
      	 
      	 m.addEventListener( "resolve", resolve );
      	 m.addEventListener( "reject", reject );
      	 
      	 m.react();
      } );
}

// ok
function firstPromise( promises ) {
   hiphop machine m( resolve, reject ) {
      fork {
      	 abort( resolve ) {
	    ${ promises.map( 
		  p => { 
		     hiphop { run (promiseToModule( acc ))( ... ) } 
		  } ); }
      	 }
      }
   }
   
   return new Promise( (resolve, reject) => {
      	 resolve 3;
      } );
}

// ok
function firstPromise( promises ) {
   return hiphop module( resolve, reject ) {
      abort( resolve ) {
      	 ${ promises.map( 
	       (p, b) => { 
	       	  hiphop { run (promiseToModule( acc ))( ... ) } 
	       } ); }
      }
   }
}

// ok
function firstPromise( promises ) {
   return hiphop module( resolve, reject ) {
      abort( resolve ) {
      	 ${ promises.map( 
	       a => { 
		  hiphop { run (promiseToModule( acc ))( ... ) } 
	       } ); }
      }
   }
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

module.exports = hiphop machine( in name = "", in passwd = "",
in login, in logout,
				 out enable, 
				 out status = "disconnected" combine( x, y ) => x + "-" + y,
                                 timeout = 0, freeze = 0 ) {
   signal connected, disabled;

   fork {
      run enableLogin( ... );
   } par {
      every( login.now ) {
	 run login( ... );
      }
   } par {
      every( connected.now ) {
	 startSession( ... )
      }
   }
}

/*---------------------------------------------------------------------*/
/*    XML                                                              */
/*---------------------------------------------------------------------*/
// bad text parsing
const login = <impress.slide title="Login"
  			     id="hh-login"
			     class="hh"
			     data-y=0>
  <div class="slide-title"><span class="titlekey">Login: a Case Study</span></div>
  <div class="javascript code">
    <tt class="filename">./specification.txt</tt>
    ${x}
  </div>
</impress.slide>

// ok
service hello() {
   /* toto */var y = 3; var x = FOO( x, y, z,
			   4, 5,
			   4 );
   try {
      foo( 1, 2, 
	 3, bar( x,
	       y ) );
      var z = <div>
	<span>
	  toto
	</span>
      </div>;
      var t = <div>
	<span3>
	  toto
	</span3>
	<span>
	  <span2>
	    <div>
	    </div>
	  </span2>
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

// ok, stand alone tags
const App = () => 
   <div>
     <h2> Shopping Cart Example</h2>
     <hr/>
     <ProductsContainer/>
     <hr/>
     <CartContainer/>
   </div>;

// ok CSS rules
#title-block-top div {
   color: red;
   text-align: center;
   line-height: 120px;
}

var web = <impress.row id="row-web"
		       class="row-stack"
		       data-scale=4
		       height=${10 + config.slideHeight}
		       data-x=${- config.slideWidth * 2}
		       data-y=${config.slideHeight * 8}>
  ${title}
</impress.row>

<style>
  #js-nodejs-module .code tt.filename {		
     background: ${theme.greyextralight};
     padding: 2px;
     position: absolute;
     top: 5px; 
     right: 5px;
     border: 1px solid ${theme.grey};
     
     font-size: 18px;
     color: ${theme.greydark};
  }
</style>

// ok: css indent
<style>
  #conclusion .slide-title {
     margin-bottom: 1ex;
  }
</style>

// ok
abro.css = <style>
  #hh-abro #listener {
     opacity: 0;
     transition: 1s all;
  }
  #hh-abro[data-step="1"] #listener {
     opacity: 1;
     transition: 1s all;
  }
</style>  

// ok
<style>
  #id .fll {
     color: red;
  }
</style>

/*---------------------------------------------------------------------*/
/*    Json                                                             */
/*---------------------------------------------------------------------*/
// ok
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

// ok, brace after colon
{ "__ast__":
   { "__node__": "J2SProgram", 
     "exports": 
	[{ 
	    "__node__": "J2SExport", 
	    "id": "default", 
	    "alias": "default", "from": false}], 
     "imports": [], 
     "source-map": false 
  } 
}
