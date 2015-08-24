/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/svg/svg.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Mon Aug 24 13:13:49 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    basic Hop.js SVG support                                         */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g svg.js                                            */
/*    browser: http://localhost:8080/hop/svg                           */
/*=====================================================================*/
var hop = require( "hop" );

service svgServerDate() {
   return Date.now();
}

service svg() {
   var path = hop.shareDir + "/icons/hop/hop.svg";
   return <html>
     <table>
       <tr>
	 <td>
	   <svg:img width="1em" height="1ex" src=${path}/>
	 </td>
	 <td>
	   <svg:img width="5em" height="5ex" src=${path}/>
	 </td>
	 <td>
	   <svg:img width="10em" height="10ex" src=${path}/>
	 </td>
       </tr>
     </table>
     <svg width="400" height="430">
       <svg:polygon style="stroke:blue; stroke-width:1.5;fill:silver"
		   points="10,10 180,10 10,250 50,50 10,10"/>
       <svg:circle style="stroke:red; stroke-width:2; fill: yellow; opacity: 0.5"
		  cx="100" cy="80" r="75"/>
       <svg:rect style="stroke:green; stroke-width:3; fill: #ded; opacity:.8"
		x="30" y="80" height="120" width="220"/>
       <svg:path style="fill:red;fill-rule:evenodd;stroke:none"
		d="M 230,250 C 360,30 10,255 110,140 z"/>
     </svg>
   </html>;
}
   
console.log( "Go to \"http://%s:%d/hop/svg\"", hop.hostname, hop.port );
