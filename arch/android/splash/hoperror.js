var path = require( "path" );

var splash = <html>
  <head rts=${false}>
    <meta name="viewport"
	  content="width=device-width, height=device-height, initial-scale=1, maximum-scale=1, user-scalable=no">
  </head>
  <body style="background-color: #222;">
    HOP ERROR
    <div style="margin-left: auto; margin-right: auto; text-align: center; margin-top: 20px;">
    HOP ERROR
      <svg:img width="30%" src=${path.dirname( module.filename ) + "/hop.svgz"}/>
      <div style="color: #fff; font-size: 14px">
	HOP.js
      </div>
    </div>
  </body>
</html>


console.log( hop.compileXML( splash ) );
