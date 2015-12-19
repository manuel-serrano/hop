/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/share/hop-require.js              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue May 27 06:09:16 2014                          */
/*    Last change :  Thu Dec 17 07:54:09 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Client side implementation of the "require" form                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_modules ...                                                  */
/*---------------------------------------------------------------------*/
// see hop-boot.js, which is included first in hop_[su].js

/*---------------------------------------------------------------------*/
/*    require ...                                                      */
/*---------------------------------------------------------------------*/
function require( url ) {
   var path = ( url.charAt( 0 ) == '.' )
       ? hop[ '%filenameCanonicalize' ]( hop[ '%root' ] + "/" + url )
       : url;

   if( window.hop[ '%modules' ][ path ] ) {
      return window.hop[ '%modules' ][ path ];
   } else {
      if( window.hop[ '%requires' ][ path ] ) {
	 return window.hop[ '%requires' ][ path ]();
      } else {
	 throw new Error( "Client-side module \"" + url + "\" not requirable" );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    %requireAlias ...                                                */
/*---------------------------------------------------------------------*/
hop[ '%requireAlias' ] = function( name, mod ) {
   window.hop[ '%requires' ][ name ] = function() {
      return require( mod );
   }
}

/*---------------------------------------------------------------------*/
/*    %filenameCanonicalize ...                                        */
/*---------------------------------------------------------------------*/
hop[ '%filenameCanonicalize' ] = function( name ) {
   var s = name.split( "/" );
   var r = [];

   for( var i = 0; i < s.length; i++ ) {
      switch( s[ i ] ) {
	 case "..": r.pop(); break;
	 case ".": break;
	 default: r.push( s[ i ] );
      }
   }

   return r.join( "/" );
}

/*---------------------------------------------------------------------*/
/*    %require ...                                                     */
/*---------------------------------------------------------------------*/
hop[ '%require' ] = function( name, mod ) {
   
   function dirname( name ) {
      var i = name.lastIndexOf( "/" );
      if( i > 0 ) {
	 return name.substring( 0, i );
      } else {
	 return name;
      }
   }

   function fileExists( file ) {
      return file in hop[ '%requires' ];
   }
   
   function resolveFile( file ) {
      if( fileExists( file ) ) {
	 return file;
      } else {
	 var filejs = file + ".js";
	 if( fileExists( filejs ) ) return filejs;
	 
	 filejs = file + ".json";
	 if( fileExists( filejs ) ) return filejs;
	 
	 return false;
      }
   }

   function resolvePackage( json, dir ) {
      var o = hop[ '%requires' ];

      if( "main" in o ) return o.main;
      var idx = dir + "/index.js";

      if( fileExists( idx ) ) return idx;
      return false;
   }
   
   function resolveDirectory( x ) {
      var json = x + "/package.json";
      
      if( fileExists( json ) ) {
	 var m = resolvePackage( json, x );

	 if( m ) return resolveFileOrDirectory( x + "/" + m );
      }

      var p = x + "/index.js";

      if( fileExists( p ) ) return p;
      
      return false;
   }
   
   function resolveFileOrDirectory( x, dir ) {
      var file = hop[ '%filenameCanonicalize' ]( dir + "/" + x );
      return resolveFile( file ) || resolveDirectory( file );
   }
   
   function resolveModules( mod, x ) {
      for( var i = 0; i < mod.paths.length; i++ ) {
	 var r = resolveFileOrDirectory( x, mod.paths[ i ] );
	 if( r ) return r;
      }
      return false;
   }

   function isCoreModule( name ) {
      return name in hop[ '%modules' ];
   }
   
   function resolveError( name ) {
      throw new Error( "Cannot require client-side module \"" + name + "\"" );
   }
   
   function resolve( name ) {
      // mimic nodejs resolver (see nodejs/require.scm: nodejs-resolve)
      if( isCoreModule( name ) ) {
	 return name;
      }
      if( name.match( /..?[/]/ ) ) {
	 return resolveFileOrDirectory( name, dirname( mod.filename ) )
	    || resolveModules( mod, name )
	    || resolveError( name );
      }
      if( name.charAt( 0 ) === '/' ) {
	 return resolveFileOrDirectory( name, "/" )
	    || resolveModules( mod, name )
	    || resolveError( name );
      }
      return resolveModules( mod, name )
	 || resolveError( name );
   }

   return require( resolve( name ) );
}
