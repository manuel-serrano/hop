/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/examples/examples.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 12 15:48:12 2014                          */
/*    Last change :  Tue Jan  6 16:13:38 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The example driver                                               */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g examples.js                                       */
/*    browser: http://localhost:8080/hop/examples                      */
/*=====================================================================*/
var hop = require( "hop" );
var fs = require( "fs" );
var path = require( "path" );
var util = require( "util" );
var spawn = require( "child_process" ).spawn;
var fontifier = require( hop.fontifier );

var PORT = hop.port + 1;
var PROCESSES = [];
var SHELL = "/bin/sh";
var CMD = "hop -g --no-color --no-zeroconf -p $(PORT) $(SRC) --rc-file $(RCFILE)";
var PASSWD = path.join( __dirname, "passwd.hop" );

var CLASSLEVELS = [ "success", "info", "warning", "danger" ];

import service examplesSrc( path );
import service examplesDoc( o );

/*---------------------------------------------------------------------*/
/*    src.js and doc.js                                                */
/*    -------------------------------------------------------------    */
/*    Spawn the examplesSrc background service.                        */
/*---------------------------------------------------------------------*/
var src = new Worker( "./src.js" );
var doc = new Worker( "./doc.js" );

/*---------------------------------------------------------------------*/
/*    examples ...                                                     */
/*    -------------------------------------------------------------    */
/*    Service driver for the Hop.js examples.                          */
/*---------------------------------------------------------------------*/
service examples() {
   
   var title = <SPAN> {};
   var description = <SPAN> { class: "description" };
   var tags = <SPAN> {};
   var doc = <DIV> {};
   var sources = <DIV> {};
   var consoles = <DIV> { class: "consoles" };
   var stitle = <SPAN> {};
   var iframe = <IFRAME> {};
   var url = <A> { class: "panel-heading-url" };
   var root = path.dirname( __dirname );

   var panel = <DIV> {
      class: "panel panel-primary",
      <DIV> {
	 class: "panel-heading",
	 <SPAN> { class: "glyphicon glyphicon-globe", "aria-hidden": "true" },
	 url
      },
      <DIV> {
	 class: "panel-body",
	 <DIV> {
	    class: "embed-responsive embed-responsive-4by3",
	    iframe
	 }
      }
   };

   return <HTML> {
      <HEAD> {
	 title: "Hop.js example driver",
	 include: "md5",
	 css: [ examples.resource( "libs/bootstrap/css/bootstrap.min.css" ),
		examples.resource( "examples.hss" ),
	        fontifier.css ],
	 <META> {
	    charset: "utf-8"
	 },
	 <META> {
	    "http-equiv": "X-UA-Compatible",
	    content: "IE=edge"
	 },
	 <META> {
	    name: "viewport",
	    content: "width=device-width, initial-scale=1"
	 },
      },
      
      <BODY> {
	 <DIV> {
	    class: "container-fluid",
	    <DIV> {
	       class: "jumbotron",
	       <H1> { "Hop v", hop.version },
	       <H2> { "Examples suite" }
	    },
	    
	    <DIV> {
	       class: "row",
	       <DIV> {
		  class: "col-md-3",
		  <UL> {
		     class: "list-group",
		     getExamples( root )
			.map( function( o ) {
			   var level = CLASSLEVELS[ Math.round( o.level ) ];
			   return <A> {
			      class: "list-group-item list-group-item-" + level,
			      onclick: ~{
				 var glyph = this.firstChild;
				 var files = ${o.files};
				 var commands = ${o.commands};
				 
				 event.stopPropagation = true;
				 
				 ${title}.className = "label label-" + ${level};
				 ${title}.innerHTML = ${o.title};
				 ${url}.innerHTML = ${o.url};
				 ${url}.href = ${o.url};
				 ${description}.innerHTML = ${o.description};
				 ${tags}.innerHTML = ${o.tags.join( ", " )};
				 ${doc}.innerHTML = "";

				 // get the documentation
				 ${examplesDoc}( ${o} )
				    .post( function( doc ) {
				       ${doc}.appendChild( doc );
				    } );
				 
				 var el = document.getElementById( "example" );
				 dom_remove_class( el, "no-display" );
				 var el = document.getElementById( "init" );
				 dom_add_class( el, "no-display" );

				 ${sources}.innerHTML = "";

				 // walk over all the sources, one after the other
				 (function loop( i ) {
				    if( i < files.length ) {
				       var s = files[ i ];
				       var box = <DIV> { class: "panel-body" };
				       var el = <DIV> {
					  class: "panel panel-default",
					  <DIV> {
					     class: "panel-heading",
					     <SPAN> { class: "glyphicon glyphicon-book", "aria-hidden": "true" },
					     s.substr( ${o.dir.length + 1} )
					  },
					  box
				       };

				       ${sources}.appendChild( el );

				       ${examplesSrc}( s )
					  .post( function( code ) {
					     box.appendChild( code );
					     loop( i + 1 );
					  } );
				    }
				 })( 0 );

				 // add the consoles
				 ${consoles}.innerHTML = "";

				 for( var i = 0; i < commands.length; i++ ) {
				    (function() {
				       var cmd = commands[ i ];
				       var pre = <PRE> { class: "console" };
				       var con = <DIV> {
					  class: "panel panel-info console",
					  <DIV> {
					     class: "panel-heading",
					     <SPAN> { class: "glyphicon glyphicon-cog", "aria-hidden": "true" },
					     cmd
					  },
					  pre
				       };

				       server.addEventListener( files[ i ], function( e ) {
					  pre.appendChild( e.value );
				       } );

				       ${consoles}.appendChild( con );
				    })();
				 }
				       
				 ${examplesRun}( ${o} )
				    .post( function( url ) {
				       if( url ) {
					  ${url}.innerHTML = url;
					  ${url}.href = url;
					  ${iframe}.src = url;
					  ${panel}.style.display = "block";
				       } else {
					  ${panel}.style.display = "none";
				       }
				       glyph.style.visibility = "visible";
				    } );
			      },
			      title: o.description,
			      <SPAN> {
				 class: "glyphicon glyphicon-ok",
				 style: "visibility: hidden"
			      },
			      <SPAN> {
				 class: "title",
				 o.title
			      }
			   }
			} )
		  }
	       } </DIV>,
	       <DIV> {
		  class: "col-md-9", id: "init",
		  "This hop.js program shows various examples, which illustrate various features of the system. Click the navigation bar to access the examples."
	       },
	       <DIV> {
		  class: "col-md-9 no-display", role: "main", id: "example",
		  <DIV> {
		     class: "panel panel-default",
		     <DIV> {
			class: "panel-heading",
			<H3> { title, description },
			<DIV> {
			   class: "tags",
			   <SPAN> {
			      class: "glyphicon glyphicon-tags"
			   },
			   tags
			}
		     },
		     <DIV> {
			class: "panel-body",
			doc,
		     }
		  },
		  sources,
		  consoles,
		  panel
	       }
	    } </DIV>
	 },	    
	 <SCRIPT> {
	    src: examples.resource( "libs/jquery/js/jquery.min.js" )
	 },
	 <SCRIPT> {
	    src: examples.resource( "libs/bootstrap/js/bootstrap.min.js" )
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    getExamples ...                                                  */
/*    -------------------------------------------------------------    */
/*    Scans the current directory to find all the examples, which      */
/*    are characterized by their example.json file.                    */
/*---------------------------------------------------------------------*/
function getExamples( dir ) {
   // extract all the example.json file, adding a
   // path prop and a title prop
   return fs.readdirSync( dir )
      .filter( function( d ) {
	 var p = dir + "/" + d;
	 return fs.statSync( p ).isDirectory()
	    && fs.existsSync( p + "/example.json" )
	    && fs.statSync( p + "/example.json" ).isFile();
      } )
      .map( function( d ) {
	 var p = dir + "/" + d;
	 var o = require( p + "/example.json" );
	 o.dir = p;

	 if( !("title" in o) ) o.title = d;
	 if( !("service" in o) ) o.service = d;
	 if( !("level" in o) ) o.level = 0.1;
	 if( !("commands" in o) ) o.commands = [ CMD ];

	 if( ("files" in o) ) {
	    o.files = o.files.map( function( el ) {
	       return dir + "/" + d + "/" + el
	    } );
	 } else {
	    o.files = [ dir + "/" + d + "/" + d + ".js" ];
	 }

	 return o;
      } )
      .sort( function( a, b ) {
	 if( a.level < b.level ) {
	    return -1;
	 }
	 if( a.level > b.level ) {
	    return 1;
	 }
	 if( a.title < b.title ) {
	    return -1;
	 }
	 if( a.title > b.title ) {
	    return 1;
	 }
	 return 0;
      } );
}

/*---------------------------------------------------------------------*/
/*    examplesRun ...                                                  */
/*---------------------------------------------------------------------*/
service examplesRun( o ) {
   var PORTINC = 5;

   PORT += PORTINC;
   
   return hop.HTTPResponseAsync(
      function( sendResponse ) {
	 function run( command, file, port, rep ) {
	    var cmd = command.replace( /[$][(]([A-Z]+)[)]/g,
				       function( patch, p ) {
					  if( p == "PORT" ) return port + "";
					  if( p == "SRC" ) return file;
					  if( p == "RCFILE" ) return path.relative( o.dir, PASSWD );
					  if( p == "PREPORT" ) return (port - PORTINC) + "";
					  if( p == "NEXTPORT" ) return (port + PORTINC) + "";
					  if( p == "DIR" ) return o.dir;
				       } );
	    var execpath = path.dirname( process.execPath );
	    var env = { PATH: execpath + ":" + process.env.PATH };
	    
	    env.__proto__ = process.env;

	    var proc = spawn( SHELL, [ "-c", cmd ], { env: env, cwd: o.dir } );

	    proc.stdout.on( "data", function( data ) {
	       hop.broadcast( file, <SPAN> { class: "stdout", data.toString() } );
	       if( rep ) {
		  if( o.service ) {
		     sendResponse( util.format( "http://localhost:%d/hop/%s", port, o.service ) );
		  } else {
		     sendResponse( false );
		  }
	       }
	    } );

	    proc.stderr.on( "data", function( data ) {
	       hop.broadcast( file, <SPAN> { class: "stderr", data.toString() } );
	    } );

	    // remove ended process from the running processes list
	    proc.on( "exit", function( p ) { delete PROCESSES[ proc ] } );

	    // store the spawned process in the running list
	    PROCESSES.push( proc );
	 };

	 // kill all the running processes
	 PROCESSES.forEach( function( p ) { p.kill( "SIGKILL" ) } );
	 PROCESSES = [];

	 // spawn all the commands
	 for( var i = 0; i < o.commands.length; i++, PORT += PORTINC ) {
	    run( o.commands[ i ], o.files[ i ], PORT, i == 0 );
	 }
      }, this );
}

console.log( "Go to \"http://%s:%d/hop/examples\"", hop.hostname, hop.port );
