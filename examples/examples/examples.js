/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/examples/examples.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 12 15:48:12 2014                          */
/*    Last change :  Tue May 23 22:06:55 2017 (serrano)                */
/*    Copyright   :  2014-17 Manuel Serrano                            */
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
var doc = require( "hopdoc" );

var PORT = hop.port + 1;
var PROCESSES = [];
var SHELL = "/bin/sh";
var CMD = "hop -g --sofile-policy none --no-color --no-zeroconf -p $(PORT) $(SRC) --rc-file $(RCFILE)";
var PASSWD = path.join( __dirname, "passwd.hop" );

var CLASSLEVELS = [ "success", "info", "warning", "danger", "default" ];

service examplesSrc();
service examplesDoc();

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
   
   var title = <span/>;
   var description = <span class="description"/>;
   var tags = <span/>;
   var docn = <div/>;
   var sources = <div>foo bar</div>;
   var consoles = <div class="consoles"/>;
   var stitle = <span/>;
   var iframe = <iframe/>;
   var url = <a  class="panel-heading-url"/>;
   var root = path.dirname( __dirname );
   
   var css = [ require.resolve( "./libs/bootstrap/css/bootstrap.min.css" ),
	       require.resolve( "./examples.hss" ),
	       fontifier.css,
	       doc.css ]

   var panel = <div class="panel panel-primary">
     <div class="panel-heading">
       <span class="glyphicon glyphicon-globe" aria-hidden="true"/>
       ${url}
     </div>
     <div class="panel-body">
       <div class="embed-responsive embed-responsive-4by3">
	  ${iframe}
       </div>
     </div>
   </div>

   return <html>
      <head title="Hop.js example driver"
	    include="md5"
	    css=${css}>
	<meta charset="utf-8"/>
	<meta http-equiv="X-UA-Compatible" content="IE=edge"/>
	<meta name="viewport" content="width=device-width, initial-scale=1"/>
	 ~{
	    var listeners = [];
	    
	    function run( event, o, level, glyph ) {
	       var files = o.files;
	       var commands = o.commands;

	       event.stopPropagation = true;

	       ${title}.className = "label label-" + level;
	       ${title}.innerHTML = o.title;
	       ${url}.innerHTML = o.url;
	       ${url}.href = o.url;
	       ${description}.innerHTML = o.description;
	       ${tags}.innerHTML = o.tags.join( ", " );
	       ${docn}.innerHTML = "";

	       // get the documentation
	       ${examplesDoc}( o )
		  .post( function( n ) {
		     ${docn}.appendChild( n );
		  } );

	       var el = document.getElementById( "example" );
	       el.className = el.className.replace( /[ \t]*no-display\b/, "" );
	       var el = document.getElementById( "init" );
	       el.className = el.className + " no-display";

	       ${sources}.innerHTML = "";
	       // walk over all the sources, one after the other
	       (function loop( i ) {
		  if( i < files.length ) {
		     var s = files[ i ];
		     var box = <div class="panel-body"/>;
		     var el = <div class="panel panel-default">
		       <div class="panel-heading">
			 <span class="glyphicon glyphicon-book" aria-hidden="true"/>
                         ${s.substr( o.dir.length + 1 )}
		       </div>
		       ${box}
		     </div>

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

	       listeners.forEach( function( ltn ) {
		  server.removeEventListener( ltn.name, ltn.proc );
	       } );
	       listeners = [];

	       for( var i = 0; i < commands.length; i++ ) {
		  (function() {
		     var cmd = commands[ i ];
		     var pre = <pre class="console"/>;
		     var con = <div class="panel panel-info console">
		       <div class="panel-heading">
			 <span class="glyphicon glyphicon-cog" aria-hidden="true"/>
                         ${cmd}
		       </div>
                       ${pre}
		     </div>;

		     // add the new listener
		     var ltn = {
			name: files[ i ],
			proc: function( e ) {
			   pre.appendChild( e.value );
			}
		     };

		     server.addEventListener( ltn.name, ltn.proc );
		     listeners.push( ltn );

		     ${consoles}.appendChild( con );
		  })();
	       }

	       ${examplesRun}( o )
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
	    }
	 }
      </head>
      
      <body>
	<div class="container-fluid">
	  <div class="jumbotron">
	    <h1>Hop v${hop.version}</h1>
	    <h2>Examples suite</h2>
	  </div>

	  <div class="row">
	    <div class="col-md-3">
	      <ul class="list-group">
		${getExamples( root )
		  .map( function( o ) {
		     var level = CLASSLEVELS[ Math.round( o.level ) ];
		     var glyph = <span class="glyphicon glyphicon-ok"
				       style="visibility: hidden"/>
		     return <a class=${"list-group-item list-group-item-" + level}
			       title=${o.description}
			       onclick=~{run( event, ${o}, ${level}, ${glyph} )}>
		       <span class="title">
			 ${glyph}
                         ${o.title}
		       </span>
		     </a>
		  })}
	      </ul>
	    </div>
	    <div class="col-md-9" id="init">
                This hop.js program shows various examples, which illustrate various
                features of the system. Click the navigation bar to access the examples.
	    </div>
	    <div class="col-md-9 no-display" role="main" id="example">
	      <div class="panel panel-default">
		<div class="panel-heading">
		  <h3> ${title} ${description} </h3>
		  <div class="tags">
		    <span class="glyphicon glyphicon-tags">
			${tags}
		    </span>
		  </div>
		</div>
		<div class="panel-body">
                      ${docn}
		</div>
	      </div>
	      ${sources}
	      ${consoles}
	      ${panel}
	    </div>
	  </div>
	</div>
	<script src=${require.resolve( "./libs/jquery/js/jquery.min.js")}/>
	<script src=${require.resolve( "./libs/bootstrap/js/bootstrap.min.js")}/>
      </body>
   </html>
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
	 
	 if( !("dir" in o) ) {
	    o.dir = p;

	    if( !("title" in o) ) o.title = d;
	    if( !("service" in o) ) o.service = d;
	    if( !("level" in o) ) o.level = 0.1;
	    if( !("commands" in o) ) o.commands = [ CMD ];
	    if( fs.existsSync( p + "/README.md" ) ) {
	       o.doc = p + "/README.md";
	    }

	    if( ("files" in o) ) {
	       o.files = o.files.map( function( el ) {
		  return dir + "/" + d + "/" + el
	       } );
	    } else {
	       o.files = [ dir + "/" + d + "/" + d + ".js" ];
	    }
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
	       hop.broadcast( file, <span class="stdout">${data.toString()}</span> );
	       if( rep ) {
		  rep = false;
		  if( o.service ) {
		     sendResponse( util.format( "http://%s:%d/hop/%s", hop.hostname, port, o.service ) );
		  } else {
		     sendResponse( false );
		  }
	       }
	    } );

	    proc.stderr.on( "data", function( data ) {
	       hop.broadcast( file, <span> class="stderr">${data.toString()}</span> );
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
