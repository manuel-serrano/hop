/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-sound.js                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio                                    */
/*    Creation    :  12-Oct-2006 10:05                                 */
/*    Last change :  Sun Jul  8 11:54:09 2007 (serrano)                */
/*    Copyright   :  2007 GPL                                          */
/*    -------------------------------------------------------------    */
/*    Sound Support for HOP                                            */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Global parameters                                                */
/*---------------------------------------------------------------------*/
var flashProxy;
var soundCounter = 0;
var soundPool = new Array();

/*---------------------------------------------------------------------*/
/*    hop_sound_init ...                                               */
/*---------------------------------------------------------------------*/
function hop_sound_init() {
   var uid = new Date().getTime();
   var uid = 23;
   var flashdir = hop_share_directory() + "/flash/";
   var tag = new FlashTag( flashdir + 'hop-sound.swf', 1, 1 ); 

   flashProxy = new FlashProxy( uid, flashdir + 'JavaScriptFlashGateway.swf' );
   tag.setFlashvars( 'lcId=' + uid );
   tag.write( document );
}

/*---------------------------------------------------------------------*/
/*    hop_make_sound ...                                               */
/*---------------------------------------------------------------------*/
function hop_make_sound( url ) {
    var snd = new Object();

    snd.id = soundCounter++;
    snd.url = url;
    snd.volume = 100;
    snd.pan = 0;
    soundPool[ snd.id ] = snd;
    
    return snd;
}

/*---------------------------------------------------------------------*/
/*    hop_sound_load ...                                               */
/*---------------------------------------------------------------------*/
function hop_sound_load( snd, streaming ) {
   var str = ((streaming == undefined) || (streaming)) ?  1 : 0;
   flashProxy.call( 'new_sound', snd.id, snd.url, str );
}


/*---------------------------------------------------------------------*/
/*    hop_sound_onLoadHandler ...                                      */
/*    -------------------------------------------------------------    */
/*    Handlers (these functions are called from AS)                    */
/*---------------------------------------------------------------------*/
function hop_sound_onLoadHandler( id, success ) {
   var snd = soundPool[id];

   if( snd.onLoad != undefined ) {
      snd.onLoad(success);
   }
}

/*---------------------------------------------------------------------*/
/*    hop_sound_onCompleteHandler ...                                  */
/*---------------------------------------------------------------------*/
function hop_sound_onCompleteHandler( id ) {
   var snd = soundPool[id];

   if( snd.onSoundComplete != undefined ) {
      snd.onSoundComplete();
   }
}

/*---------------------------------------------------------------------*/
/*    Initialization                                                   */
/*---------------------------------------------------------------------*/
hop_sound_init();
