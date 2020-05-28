"use strict";

import { qrcode as Qrcode } from "./qrcode-lib.js";

export function QRCODE( attr, ...nodes ) {
   const el = DIV( attr );
   const lt = ~{ window.addEventListener( "load", function() { 
      document.qrcode( ${el.id}, ${attr } );
   } );
   };
   return [ lt, el ];
}

export function qrcode( el, attr ) {
   var qr = Qrcode( attr.type || 4, attr.level || 'L'  );
   qr.addData( attr.data );
   qr.make();
   document.getElementById( el ).innerHTML = qr.createImgTag( attr.pixelSize || 4 );
}

		
if( !hop.isServer ) document.qrcode = qrcode;
