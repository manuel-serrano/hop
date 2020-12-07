var path = require( "path" );

var splash = <html>
  <head rts=${false}>
    <meta name="viewport"
	  content="width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=no">
    <style>
      :visited {color: #777;}
      a {color: #ccc;}
    </style>
  </head>
  <body style="background-color: #222; color: #eee">
    <div style="margin-left: auto; margin-right: auto; text-align: center; margin-top: 20px;">
      <svg:img width="30%" src=${path.dirname( module.filename ) + "/hop.svgz"}/>
      <div style="font-size: 18px; font-weight: bold">
	HOP.js
      </div>
    </div>
    <div style="margin-left: auto; margin-right: auto; text-align: center; margin-top: 20px;">
      Hop is not install on your device. It needs to be installed first.
    </div>
    <div style="margin-left: auto; margin-right: auto; text-align: center; margin-top: 20px;">
      Hop can be installed at

      <div style="margin-top: 2ex; margin-bottom: 2ex">
      	<a href="ftp://ftp-sop.inria.fr/indes/fp/Hop/hop.apk">
	  <button style="color: white; background-color: #FAC700; border-radius: 4px; border: 1px solid #888; padding: 8px 32px 8px 32px">
	    <svg:img width="2em" src=${path.dirname( module.filename ) + "/download.svg"}/>
	    <span style="font-weight: bold; padding-left: 0.5em">hop.apk</span>
	  </button>
      	</a>
      </div>
      
    </div>
    <div style="margin-left: auto; margin-right: auto; text-align: center; margin-top: 10px;">
      Visit <a href="http://hop.inria.fr">hop.inria.fr</a> for other versions or information
      regarding Hop.
    </div>
  </body>
</html>

console.log( hop.compileXML( splash ) );
