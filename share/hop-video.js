/*=====================================================================*/
/*    serrano/prgm/project/hop/1.9.x/share/hop-video.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jan 15 08:07:15 2008                          */
/*    Last change :  Tue Jan 15 08:11:30 2008 (serrano)                */
/*    Copyright   :  2008 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    HOP client-side video support.                                   */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_video_init ...                                               */
/*---------------------------------------------------------------------*/
function hop_video_init( id, src ) {
   alert( "hop_video_init: id=" + id + " src=" + src );
}


/*---------------------------------------------------------------------*/
/*    hop_audio_flash_init ...                                         */
/*---------------------------------------------------------------------*/
function hop_audio_flash_init( id, src ) {
   /* we are now sure that at least version 8 of flash is running */
   hop_flash_minversion_set( 8 );

   alert( "hop_audio_flash_init: id=" + id + " src=" + src );
}
