/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/weblets/hopdroid/auth.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Dec 14 16:31:22 2020                          */
/*    Last change :  Mon Dec 14 16:31:23 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Access authentication                                            */
/*=====================================================================*/
"use hopscript"

/*---------------------------------------------------------------------*/
/*    module                                                           */
/*---------------------------------------------------------------------*/
const fs = require( 'fs' );
const path = require( 'path' );

import { config, update as updateConfig } from './config.js';
import { phone } from './phone.js';
import * as localConfig from './config.js';

/*---------------------------------------------------------------------*/
/*    cookieKey                                                        */
/*---------------------------------------------------------------------*/
const cookie = { 
   seed: hop.loginCookieCryptKey,
   key: "hopdroid" + hop.port, 
   _val: undefined, 
   get val() { 
      if( this._val ) { 
	 return this._val;
      } else {
	 this._val = hop.sha1sum( `${cookie.seed}:${phone.name}:${localConfig.password}` );
	 return this._val;
      }
   },
   HTTPValue: function() {
      return `${this.key}==${this.val}; Path=/; HttpOnly`;
   },
   HTTPInvalidate: function() {
      return `${this.key}=; Path=/; HttpOnly; Max-Age=0, Expires=Thu, 01 Jan 1970 00:00:00 GMT`;
   }
}

/*---------------------------------------------------------------------*/
/*    authorizeRequest ...                                             */
/*---------------------------------------------------------------------*/
export function authorizeRequest( req, rep ) {
   if( hop.isLocalRequest( req ) || !localConfig.password || checkRequestCookie( req ) ) {
      return rep( req );
   } else if( checkRequestAuthorization( req ) ) {
      return hop.HTTPResponseXml( rep( req ), {
	 contentType: "text/html", 
	 header: { "Set-Cookie": cookie.HTTPValue() }
      } );
   } else {
      return hop.HTTPResponseString( "Authentication required", { 
	 startLine: "HTTP/1.0 401 Unauthorizedx",
	 header: { 
	    "WWW-Authenticate": 'Basic realm="Basic hop authentication"',
	    "Set-Cookie": cookie.HTTPInvalidate() }
      } );
   }
}

/*---------------------------------------------------------------------*/
/*    checkequestCookie ...                                            */
/*---------------------------------------------------------------------*/
function checkRequestCookie( req ) {
   const cookie = req.header.cookie;
   const m = cookie && cookie.match( "(?:[^;]*;[ ]*)*([^;=]+)=([^;:]+)");
   return (m && m[ 1 ] === cookie.key && m[ 2 ] === cookie.val );
}

/*---------------------------------------------------------------------*/
/*    checkRequestAuthorization ...                                    */
/*---------------------------------------------------------------------*/
function checkRequestAuthorization( req ) {
   if( req.authorization ) {
      const m = req.authorization.match( /^Basic (.*)$/ );
      
      if( m ) {
   	 return hop.sha1sum( `${cookie.seed}:${hop.base64decode( m[ 1 ] )}`)  === cookie.val;
      } else {
	 return false;
      }
   } else {
      return false;
   }
}
