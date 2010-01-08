/*=====================================================================*/
/*    serrano/prgm/project/hop/2.0.x/weblets/hz/hz-install.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Nov 16 11:07:40 2007                          */
/*    Last change :  Tue Dec 22 06:12:46 2009 (serrano)                */
/*    Copyright   :  2007-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Create a click and install panel                                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_install_stamp ...                                            */
/*---------------------------------------------------------------------*/
var hop_install_stamp = 0;

/*---------------------------------------------------------------------*/
/*    hop_install_weblet ...                                           */
/*---------------------------------------------------------------------*/
function hop_install_weblet( url, srci, srcr, parent ) {
   var srci = srci ? srci : 'hz-install.png';
   var srcr = srcr ? srcr : 'hz-run.png';
   var base = url;
   var i = url.lastIndexOf( '/' );
   if( i >= 0 ) base = url.substring( i + 1, url.length );

   var m = base.match( "((.*)-[0-9]+[.][0-9]+(?:[.][0-9]+)?(?:-(?:r|rc|pre)[0-9]+)?).hz" );

   if( !m ) {
      alert( "Illegal weblet base name \"" + base + "\"" );
      return ;
   }
   
   var namev = (m.length > 1) ? m[ 1 ] : base;
   var name = (m.length > 2) ? m[ 2 ] : namev;

   hop_install_stamp++;
   
   var oninstall = 'var host = document.getElementById( "hop_click_and_install_host_' + hop_install_stamp + '" );' +
      'var port = document.getElementById( "hop_click_and_install_port_' + hop_install_stamp + '" );' +
      'this.href = "http://" + host.value + ":" + port.value + ' +
      '"/hop/hz/install?url=' + encodeURIComponent( url ) + '";';
   var onrun = 'var host = document.getElementById( "hop_click_and_install_host_' + hop_install_stamp + '" );' +
      'var port = document.getElementById( "hop_click_and_install_port_' + hop_install_stamp + '" );' + 
      'this.href = "http://" + host.value + ":" + port.value + ' +
      '"/hop/hz/run?url=' + encodeURIComponent( url ) + '";';

   var host = "<table style='font-size: 60%'>" +
      "<tr><td colspan='2' width='100%'><div style='width: 100%; border-bottom: 1px solid #777'>HOP</td></tr>" +
      "<tr><th>host:</th><td><input id='hop_click_and_install_host_" + hop_install_stamp + "' size='10 type='text' value='localhost' style='border: 1px solid #bbb; color: #555; font-size: inherit'/></td></tr>" +
      "<tr><th>port:</th><td><input id='hop_click_and_install_port_" + hop_install_stamp + "' size='4 type='text' value='8080' style='border: 1px solid #bbb; color: #555; font-size: inherit'/></td></tr></table>";
   var panel = "<table class='hz-install' style='background: #daffbd; border: 1px solid #bbb; -moz-border-radius: 0.5em; font-family: arial; padding-left: 1em; padding-right: 1em'>" +
      "<tr><th colspan='3' style='font-size: 110%' align='left'>" + namev + ".hz</th></tr>" + 
      "<tr>" +
      "<td><table style='border-collapse: collapse'>" +
      "<tr><td style='text-align: center'>" +
      "<a href='' target='_blank' onclick='" + oninstall + "'>" +
      "<img class='hop_hz_click' src='" + srci + "' alt='Click and install' title='Install or update the weblet on the Hop Broker'/>" +
      "</a>" +
      "</td></tr>" +
      "<tr><td style='text-align: center; font-size: 70%'>Install / Update</td></tr>" +
      "</table></td>" +
      "<td><table style='border-collapse: collapse'>" +
      "<tr><td style='text-align: center'>" +
      "<a href='' target='_blank' onclick='" + onrun + "'>" +
      "<img class='hop_hz_click' src='" + srcr + "' alt='Click and run' title='Run the weblet on the Hop Broker'/>" +
      "</a>" +
      "</td></tr>" +
      "<tr><td style='text-align: center; font-size: 70%'>Run</td></tr>" +
      "</table></td>" +
      "<td valign='bottom'>" + host + "</td></tr></table>";
   
   var head = document.getElementsByTagName( "head" )[ 0 ];
   var style = document.createElement( 'style' );
   var css = document.createTextNode(
			 'img.hop_hz_click:hover { border: 1px solid #bbb; background: #eaffcd; }' +
			 'img.hop_hz_click { border: 1px solid transparent; }' );
   try {
      style.appendChild( css );
   } catch( e ) {
      // IE 7 (always him), raises an exception when adding a child to
      // the style element...
   }

   head.appendChild( style );

   if( parent ) {
      if( (parent instanceof String) || (typeof parent == "string") ) {
	 parent = document.getElementById( parent );
      }

      var div = document.createElement( "div" );
      div.innerHTML = panel;
      
      parent.appendChild( div );
   } else {
      document.write( panel );
   }
}
