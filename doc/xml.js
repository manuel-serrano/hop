/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/doc/xml.js                        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Aug  1 10:22:56 2015                          */
/*    Last change :  Tue Aug  4 16:21:51 2015 (serrano)                */
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
   <div class="row">
     <div class="col-md-2">
       <svg:img
         src=${path.join( attrs.root, "../share/icons/hop/hop.svg" )}
         height="20ex" width="10em"/>
      </div>
     <div class="col-md-10">
       <h1>
         Hop.js <small>/${subtitle}</small>
       </h1>
       <p>
         <span class="label label-default lbl-lg">version ${hop.version}</span>
       </p>
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
/*    navchapters ...                                                  */
/*---------------------------------------------------------------------*/
function navchapters( attrs, chapters ) {
   if( !(chapters instanceof Array) ) { chapters = arguments[ 2 ]; }

    return <ul class="nav nav-pills">
       ${chapters.map( function( p ) {
          var clazz = p.name.toLowerCase()==attrs.key
	      ? "btn-primary" : "";
	  
          return <li role="presentation" class="active">
	      <navbut class=${clazz}>
  	        <a href=${p.href}>
	          <span class=${"glyphicon " + p.icon}></span> ${p.name}
	        </a>
                <ul class="dropdown-menu">
                  <li><a href=${p.href}>${p.name}</a></li>
                  <li role="separator" class="divider"></li>
	          ${p.entries.map( function( e ) {
                       return <li><a href=${e.href}>${e.title}</a></li>
		      } )}
                </ul>
	      </navbut>
	    </li>} )}
      </ul>
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
     <div class="copyright">Copyright Inria ${copyrightYears( 2006 )}  
       <button class="inria">
         <a href="http://www.inria.fr">
           <svg:img class="inria"
                      src=${path.join( attrs.root, "../share/icons/hop/inria.svg" )}
                   height="1.5ex" width="4em"/>
         </a>
       </button>
     </div>
   </footer>
}

/*---------------------------------------------------------------------*/
/*    downloadButton ...                                               */
/*---------------------------------------------------------------------*/
function downloadButton( attrs ) {
   console.error( "href=" + path.basename( attrs.href ) );
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
/*    exports                                                          */
/*---------------------------------------------------------------------*/
exports.title = title;
exports.navbut = navbut;
exports.navchapters = navchapters;
exports.footer = docfooter;
exports.copyrightYears = copyrightYears;
exports.downloadButton = downloadButton;
