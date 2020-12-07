var path = require( "path" );

var splash = <html>
  <head rts=${false}>
    <meta name="viewport"
	  content="width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=no">
  </head>
  <body style="background-color: #222;">
    <div style="margin-left: auto; margin-right: auto; text-align: center; margin-top: 20px;">
      <svg:img width="30%" src=${path.dirname( module.filename ) + "/hop.svgz"}/>
      <div style="color: #FAC700; font-size: 14px">
	HOP.js
      </div>
    </div>
    <div style="margin-left: auto; margin-right: auto; text-align: center; margin-top: 20px;">
      Hop is not install on your device. It needs to be installed first.
    </div>
    <div style="margin-left: auto; margin-right: auto; text-align: center; margin-top: 20px;">
      Hop can be installed at
      <a href="ftp://ftp-sop.inria.fr/indes/fp/Hop/hop.apk">hop.apk</a>
    </div>
    <div style="margin-left: auto; margin-right: auto; text-align: center; margin-top: 10px;">
      Visit <a href="http://hop.inria.fr">hop.inria.fr</a> for other versions or information
      regarding Hop.
    </div>
  </body>
</html>


console.log( hop.compileXML( splash ) );
