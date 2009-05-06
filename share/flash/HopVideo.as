/*=====================================================================*/
/*    serrano/prgm/project/hop/1.9.x/share/flash/HopVideo.as           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Aug 23 16:16:58 2007                          */
/*    Last change :  Tue Jan 15 09:24:06 2008 (serrano)                */
/*    Copyright   :  2007-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HopVideo flash support.                                          */
/*                                                                     */
/*    To be compiled with:                                             */
/*     mtasc -swf HopVideo.swf -main HopVideo.as -version 9 -cp std8   */
/*    The mtasc compiler can be found at: http://www.mtasc.org/        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
import flash.external.ExternalInterface;
import mx.video.VideoPlayer;

/*---------------------------------------------------------------------*/
/*    HopVideo ...                                                     */
/*---------------------------------------------------------------------*/
class HopVideo {
   static var app : HopVideo;

   function HopVideo( init ) {
/*       var snd = new Sound();                                        */
/*                                                                     */
      // debug
      var alert = function( s ) {
	 return ExternalInterface.call( "alert", s );
      }

	       
      // External interface binding

      return ExternalInterface.call( init );
   }

   // entry point
   static function main( mc ) {
/*       var vp = new VideoPlayer();                                   */
/*       var my_video:Video; //my_video is a Video object on the Stage */
/*       ExternalInterface.call( "alert", "MAIN.1" );                  */
/*       var active_cam:Camera = Camera.get();                         */
/*       my_video.attachVideo(active_cam);                             */
/*       ExternalInterface.call( "alert", "camera=" + active_cam );    */
      
/*       var my_video:Video; // my_video is a Video object on the Stage */
/* var my_nc:NetConnection = new NetConnection();                      */
/* my_nc.connect(null);                                                */
/* var my_ns:NetStream = new NetStream(my_nc);                         */
/* my_video.attachVideo(my_ns);                                        */
/* my_ns.play("foo.flv");                                              */
/*                                                                     */
/* // Create a NetConnection object                                    */
/*       var my_nc:NetConnection = new NetConnection();                */
/* // Create a local streaming connection                              */
/*       my_nc.connect(null);                                          */
/* // Create a NetStream object and define an onStatus() function      */
/*       var my_ns:NetStream = new NetStream(my_nc);                   */
/* {*       my_ns.onStatus = function(infoObject:Object):Void {           *} */
/* {* 	 status_txt.text += "status (" + this.time + " seconds)\n";    *} */
/* {* 	 status_txt.text += "\t Level: " + infoObject.level + "\n";    *} */
/* {* 	 status_txt.text += "\t Code: " + infoObject.code + "\n\n";    *} */
/* {*       };                                                            *} */
/* // Attach the NetStream video feed to the Video object              */
/*       my_video.attachVideo(my_ns);                                  */
/* // Set the buffer time                                              */
/*       my_ns.setBufferTime(5);                                       */
/* // Begin playing the FLV file                                       */
/*       my_ns.play("http://serrano:bigloo foo.scm@localhost:8080/users/serrano/prgm/project/hop/tmp/video/mvi_2521.flv"); */
/*                                                                     */
/*                                                                     */
/*       app = new HopVideo( _root.arg );                              */
   }
}
