/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/nodejs/hophz.js                   */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 23 12:59:44 2014                          */
/*    Last change :  Fri Jul 25 16:24:13 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    HopHz binding                                                    */
/*=====================================================================*/
var hop = require( "hop" );

/*---------------------------------------------------------------------*/
/*    getHostOption ...                                                */
/*---------------------------------------------------------------------*/
function getHostOption( host, port ) {
   return {
      async: false,
      port: (port ? port : hop.port),
      host: (host ? host : hop.hostname),
      fail: function( e ) { throw e; }
   }
}
      
/*---------------------------------------------------------------------*/
/*    listCategories                                                   */
/*---------------------------------------------------------------------*/
import service HZListCategories();
HZListCategories.path = "/hop/hz/list/categories";

function listCategories( host, port ) {
   return HZListCategories()
      .post( function( v ) {
	 return v;
      }, getHostOption( host, port ) );
}

/*---------------------------------------------------------------------*/
/*    listWeblets                                                      */
/*---------------------------------------------------------------------*/
import service HZListWeblets();
HZListWeblets.path = "/hop/hz/list/weblets";

function listWeblets( category, host, port ) {
   return HZListWeblets( category )
      .post( function( v ) {
	 return v;
      }, getHostOption( host, port ) );
}

/*---------------------------------------------------------------------*/
/*    install                                                          */
/*---------------------------------------------------------------------*/
import service HZInstall();
HZInstall.path = "/hop/hz/install";

function install( url, host, port ) {
   return HZInstall( { url: url } )
      .post( function( v ) {
	 if( typeof( v ) === "pair" ) {
	    if( v.car == 404 ) {
	       throw new Error( 'Weblet "' + url + '" not found' );
	    } else {
	       throw new Error( 'Installation of "' + url
				+ '" failed with status '
				+ v.car );
	    }
	 } else {
	    return true;
	 }
      }, getHostOption( host, port ) );
}

/*---------------------------------------------------------------------*/
/*    download                                                         */
/*---------------------------------------------------------------------*/
import service HZDownload();
HZDownload.path = "/hop/hz/download";

function download( url, host, port ) {
   return HZDownload( { url: url } )
      .post( function( v ) {
	 if( typeof( v ) === "pair" ) {
	    if( v.car == 404 ) {
	       throw new Error( 'Weblet "' + url + '" not found' );
	    } else {
	       throw new Error( 'Installation of "' + url
				+ '" failed with status '
				+ v.car );
	    }
	 } else {
	    return true;
	 }
      }, getHostOption( host, port ) );
}

/*---------------------------------------------------------------------*/
/*    uninstall                                                        */
/*---------------------------------------------------------------------*/
import service HZUninstall();
HZInstall.path = "/hop/hz/uinstall";

function uninstall( weblet, host, port ) {
   return HZUninstall( { weblet: weblet } )
      .post( function( v ) {
	 return v;
      }, getHostOption( host, port ) );
}

/*---------------------------------------------------------------------*/
/*    search                                                           */
/*---------------------------------------------------------------------*/
import service HZSearch();
HZSearch.path = "/hop/hz/search/weblets";

function search( regexp, host, port ) {
   return HZSearch( { regexp: regexp } )
      .post( function( v ) {
	 if( typeof v === "pair" ) {
	    return v.toArray();
	 } else {
	    return v;
	 }
      }, getHostOption( host, port ) );
}

/*---------------------------------------------------------------------*/
/*    find                                                             */
/*---------------------------------------------------------------------*/
import service HZFind();
HZFind.path = "/hop/hz/find/weblet";

function find( name, host, port ) {
   return HZFind( { name: name } )
      .post( function( v ) {
	 return v;
      }, getHostOption( host, port ) );
}

/*---------------------------------------------------------------------*/
/*    update                                                           */
/*---------------------------------------------------------------------*/
import service HZUpdate();
HZUpdate.path = "/hop/hz/update";

function update( host, port ) {
   return HZUpdate()
      .post( function( v ) {
	 return v;
      }, getHostOption( host, port ) );
}

/*---------------------------------------------------------------------*/
/*    listUpdate                                                       */
/*---------------------------------------------------------------------*/
import service HZListUpdate();
HZListUpdate.path = "/hop/hz/list/update";

function listUpdate( host, port ) {
   return HZListUpdate()
      .post( function( v ) {
	 return v;
      }, getHostOption( host, port ) );
}

/*---------------------------------------------------------------------*/
/*    exports                                                          */
/*---------------------------------------------------------------------*/
exports.listCategories = listCategories;
exports.listWeblets = listWeblets;
exports.download = download;
exports.install = install;
exports.uninstall = uninstall;
exports.search = search;
exports.find = find;
exports.update = update;
exports.listUpdate = listUpdate;
   

