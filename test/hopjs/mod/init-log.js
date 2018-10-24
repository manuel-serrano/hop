export let allLog = {};

export function log( msg, key ) {
   if( !(key in allLog ) ) {
      allLog[ key ] = msg;
   } else {
      allLog[ key ] += " " + msg;
   }
}

export function getLog( key ) {
   return allLog[ key ];
}

export function logReset( key ) {
   allLog[ key ] = "";
}
