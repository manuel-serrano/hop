/*=====================================================================*/
/*    serrano/prgm/project/hop/weblets/hz/hz-install.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Nov 16 11:07:40 2007                          */
/*    Last change :  Fri Nov 16 16:39:44 2007 (serrano)                */
/*    Copyright   :  2007 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Create a click and install panel                                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_install_weblet ...                                           */
/*---------------------------------------------------------------------*/
function hop_install_weblet( url, src ) {
   var src = src ? src : 'hz-install.png';
   var base = url;
   var i = url.lastIndexOf( '/' );
   if( i >= 0 ) base = url.substring( i + 1, url.length );

   var m = base.match( "((.*)-[0-9]+[.][0-9]+(?:[.][0-9]+)?(?:-(?:r|rc|pre)[0-9]+)?).hz" );
   var namev = (m.length > 1) ? m[ 1 ] : base;
   var name = (m.length > 2) ? m[ 2 ] : namev;

   var onclick = 'var host = document.getElementById( "hop_click_and_install_host" );' +
      'var port = document.getElementById( "hop_click_and_install_port" );' +
      'document.location = "http://" + host.value + ":" + port.value + ' +
      '"/hop/hz/install-ondemand?name=' + name + '&url=' + url + '";';

   var host = "<table style='font-size: 60%'>" +
      "<tr><td colspan='2' width='100%'><div style='width: 100%; border-bottom: 1px solid #777'>HOP</td></tr>" +
      "<tr><th>host:</th><td><input id='hop_click_and_install_host' size='10 type='text' value='localhost' style='border: 1px solid #bbb; color: #555; font-size: inherit'/></td></tr>" +
      "<tr><th>port:</th><td><input id='hop_click_and_install_port' size='4 type='text' value='8080' style='border: 1px solid #bbb; color: #555; font-size: inherit'/></td></tr>";
   var panel = "<table style='background: #daffbd; border: 1px solid #bbb; -moz-border-radius: 0.5em; font-family: arial; padding-left: 1em; padding-right: 1em'>" +
      "<tr><th colspan='2' style='font-size: 110%' align='left'>" + namev + "</th></tr>" + 
      "<tr><td>" +
      "<table style='border-collapse: collapse'>" +
      "<tr><td align='center' style='font-size: 70%'>Install / Update</td></tr>" +
      "<tr><td align='center'>" +
      "<img id='hop_click_and_install_img' src='hz-install.png' alt='Click and install' title='Install or update package' onclick='" + onclick + "'/>" +
      "</td></tr>" +
      "</table>" +
      "</td><td valign='bottom'>" + host + "</td></tr></table>";
   
   var head = document.getElementsByTagName( "head" )[ 0 ];
   var style = document.createElement( 'style' );
   style.appendChild( document.createTextNode(
			 'img#hop_click_and_install_img:hover { border: 1px solid #bbb; background: #eaffcd; }' +
			 'img#hop_click_and_install_img { border: 1px solid transparent; }' ) );
   head.appendChild( style );

   document.write( panel );
}
