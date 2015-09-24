/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/doc/xml.js                        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Aug  1 10:22:56 2015                          */
/*    Last change :  Thu Sep 24 11:07:31 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hop.js XML extensions                                            */
/*=====================================================================*/
"use hopscript"

/*---------------------------------------------------------------------*/
/*    imports                                                          */
/*---------------------------------------------------------------------*/
const hop = require( "hop" );
const path = require( "path" );

/*---------------------------------------------------------------------*/
/*    title ...                                                        */
/*---------------------------------------------------------------------*/
function title( attrs, subtitle ) {
   return <div class="jumbotron">
   <div class="container">
   <div class="row">
     <div class="col-md-2">
       <svg:img
         src=${path.join( attrs.root, "../share/icons/hop/hop.svg" )}
         height="20ex" width="10em"/>
      </div>
     <div class="col-md-10">
       <h1>
         Hop.js ${subtitle ? <small>/${subtitle}</small> : ""}
       </h1>
       <p>
         <span class="label label-default lbl-lg">version ${hop.version}</span>
       </p>
     </div>
   </div>
   </div>
   </div>;
}

/*---------------------------------------------------------------------*/
/*    xmlNodes ...                                                     */
/*---------------------------------------------------------------------*/
function xmlNodes( arguments ) {
   var nodes = Array.prototype.slice.call( arguments, 1 );
   
   function pred( el ) {
      return !(typeof( el ) == "string" );
   }
   
   return Array.prototype.filter.call( nodes, pred );
}

/*---------------------------------------------------------------------*/
/*    navbut ...                                                       */
/*---------------------------------------------------------------------*/
function navbut( attrs, _ ) {
   const body = xmlNodes( arguments );

   return <div class="btn-group navbut">
       <button type="button"
               class=${"btn " + attrs.class}>
         ${body[ 0 ]}
       </button>
       <button type="button"
               class=${"btn dropdown-toggle " + attrs.class}
               data-toggle="dropdown"
               aria-haspopup="true"
               aria-expanded="false">
         <span class="caret"></span>
         <span class="sr-only">Toggle Dropdown</span>
        </button>
        ${body[ 1 ]}
     </div>
}

/*---------------------------------------------------------------------*/
/*    navbar ...                                                       */
/*---------------------------------------------------------------------*/
function navbar( attrs, chapters ) {
   if( !(chapters instanceof Array) ) { chapters = arguments[ 2 ]; }

   return <nav class="navbar navbar-inverse navbar-fixed-top">
     <div class="container">
       <ul class="nav navbar-nav">
         ${chapters.map( function( p ) {
	    if( p.entries.length == 0 ) {
               var clazz = p.name.toLowerCase()==attrs.key
		   ? "active" : "";
               return <li class=${clazz}>
	         <a href=${p.href}>
	           <span class=${"glyphicon " + p.icon}></span>
		   ${p.name}
	         </a>
	       </li>;
	    } else {
               var clazz = p.name.toLowerCase()==attrs.key
		   ? "dropdown active" : "dropdown";
               return <li class=${clazz}>
	         <a href=${p.href}
	            class="dropdown-toggle" data-toggle="dropdown" role="button"
	            aria-haspopup="true" aria-expanded="false">
	          <span class=${"glyphicon " + p.icon}></span> ${p.name} <span class="caret"></span>
	         </a>
                 <ul class="dropdown-menu">
                   <li><a href=${p.href}>${p.name}</a></li>
                   <li role="separator" class="divider"></li>
	           ${p.entries.map( function( e ) {
                        return <li><a href=${e.href}>${e.title}</a></li>
		   } )}
                 </ul>
	       </li>
	    } } )}
	  </ul>
     </div>
   </nav>
}

/*---------------------------------------------------------------------*/
/*    copyrightYears ...                                               */
/*---------------------------------------------------------------------*/
function copyrightYears( iyear ) {
   var y = new Date().getFullYear();

   if( y == iyear ) {
      return iyear + "";
   } else if( y == iyear + 1 ) {
      return iyear + "-" + y;
   } else {
      return iyear + "-" + y;
   }
}
   
/*---------------------------------------------------------------------*/
/*    docfooter ...                                                    */
/*---------------------------------------------------------------------*/
function docfooter( attrs ) {
   return <footer>
     <div class="container">
       <div class="copyright col-md-2 copyright-left">
       &copy; ${copyrightYears( 2006 )}
	 <a href="http://www.inria.fr">Inria</a>
       </div>
       <div class="copyright col-md-8 copyright-middle">
         <a class="iddn" href="http://app.legalis.net/">
           IDDN.FR.001.260002.000.S.P.2006.000.10400
	 </a>
         - 
         <a class="iddn" href="http://app.legalis.net/">
           IDDN.FR.001.260002.001.S.A.2006.000.10600
	 </a>
       </div>
       <div class="copyright copyright-right col-md-2">
	 <button type="button" class="inria btn btn-danger">
           <a href="http://www.inria.fr">
           <svg:img class="inria"
		    src=${path.join( attrs.root, "../share/icons/hop/inria.svg" )}
		    height="1.6ex" width="4em"/>
           </a>
	 </button>
       </div>
     </div>
   </footer>
}

/*---------------------------------------------------------------------*/
/*    downloadButton ...                                               */
/*---------------------------------------------------------------------*/
function downloadButton( attrs ) {
   return <button class=${"download btn btn-" + attrs.class}>
      <a href=${attrs.href}>
       <table>
         <tr>
           <td rowspan=2><span class=${"glyphicon " + attrs.icon}/></td>
           <td class="content">${path.basename(attrs.href)}</td>
         </tr>
       </table>
     </a>
   </button>;
}

/*---------------------------------------------------------------------*/
/*    idx ...                                                          */
/*---------------------------------------------------------------------*/
function idx( attrs, entries ) {
   return <table>
     ${entries.map( function( e ) {
	return <tr><td><a href=${e.url}>${e.key}</a></td></tr>; } )}
	</table>;
}

/*---------------------------------------------------------------------*/
/*    exports                                                          */
/*---------------------------------------------------------------------*/
exports.TITLE = title;
exports.NAVBUT = navbut;
exports.NAVBAR = navbar;
exports.FOOTER = docfooter;
exports.DOWNLOADBUTTON = downloadButton;
exports.copyrightYears = copyrightYears;
exports.IDX = idx;
