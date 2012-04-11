/*=====================================================================*/
/*    serrano/prgm/project/hop/linux/maemo/hop-launcher/hop.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 14 07:53:58 2007                          */
/*    Last change :  Sun Nov 29 20:12:25 2009 (serrano)                */
/*    Copyright   :  2007-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Run Hop                                                          */
/*=====================================================================*/
#include "config.h"
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#if( defined( MAEMO4 ) || defined( MAEMO5 ) )
#  include <hildon/hildon-program.h>
#  include <hildon/hildon-banner.h>
#else
#  include <hildon-widgets/hildon-program.h>
#  include <hildon-widgets/hildon-banner.h>
#endif
#include <gtk/gtk.h>
#include <glib.h>
#include "hop.h"
#include "gui.h"
#include "param.h"

/*---------------------------------------------------------------------*/
/*    static char **                                                   */
/*    split_string ...                                                 */
/*---------------------------------------------------------------------*/
static char **
split_string( char *cmd ) {
   static char *buf[ 255 ];
   int i = 1;

   if( (buf[ 0 ] = strtok( cmd, " " )) ) {
      while( (buf[ i++ ] = strtok( 0, " " )) );

      return buf;
   }

   return 0L;
}

/*---------------------------------------------------------------------*/
/*    static char *                                                    */
/*    get_hop_command() ...                                            */
/*---------------------------------------------------------------------*/
static char *
get_hop_command( char *cmd ) {
   static char buffer[ 1024 ];

   sprintf( buffer, cmd, hop_port );

   return &buffer[ 0 ];
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    exec ...                                                         */
/*---------------------------------------------------------------------*/
static int
exec( char *cmd ) {
   char **cmd_line = split_string( cmd );
   
   return execvp( cmd_line[ 0 ], cmd_line );
}

/*---------------------------------------------------------------------*/
/*    Global variables                                                 */
/*---------------------------------------------------------------------*/
static pid_t hop_pid = 0;

static int hop_pipe_stdout[ 2 ];
static int hop_pipe_stderr[ 2 ];

static GIOChannel *hop_stderr = 0L, *hop_stdout = 0L;

static GString *buffer;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    init_hop_io ...                                                  */
/*---------------------------------------------------------------------*/
void
init_hop_io() {
   /* Initialize the IO buffer */
   buffer = g_string_sized_new( BUFSIZ + 1 );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    close_io_channels ...                                            */
/*---------------------------------------------------------------------*/
static void
close_io_channels() {
   if( hop_stdout ) g_io_channel_close( hop_stdout );
   if( hop_stderr ) g_io_channel_close( hop_stderr );

   close( hop_pipe_stdout[ 0 ] );
   close( hop_pipe_stdout[ 1 ] );
   close( hop_pipe_stderr[ 0 ] );
   close( hop_pipe_stderr[ 1 ] );
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    watch_hup ...                                                    */
/*---------------------------------------------------------------------*/
static gboolean
watch_hup( GIOChannel *chann, GIOCondition condition, gpointer data ) {
   if( hop_pid ) {
      hop_pid = 0;

      gui_message( "Hop has terminated unexpectidely" );
      close_io_channels();
   }
   
   return FALSE;
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    watch_output ...                                                 */
/*---------------------------------------------------------------------*/
static gboolean
watch_output( GIOChannel *chann, GIOCondition condition, gpointer data ) {
   gchar buf[ BUFSIZ + 1 ];
   gsize n;
   GError *err = NULL;

   gui_stop_waiting();
   
   if( condition & G_IO_HUP )
      return watch_hup( chann, condition, data );
   
   while( G_IO_STATUS_NORMAL ==
	  g_io_channel_read_line_string( chann, buffer, 0, &err ) ) {
      hop_console_insert( buffer->str, buffer->len );
      buffer->str[ buffer->len ] = 0;
   }
      
   while( G_IO_STATUS_NORMAL ==
	  g_io_channel_read_chars( chann, buf, BUFSIZ, &n, &err ) ) {
      hop_console_insert( buf, n );
      buffer->str[ n ] = 0;
      if( n < BUFSIZ ) break;
   }
   
   hop_console_adjust();
   
   return TRUE;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    init_io_channels ...                                             */
/*---------------------------------------------------------------------*/
static void
init_io_channels() {
   GError *err = NULL;
   
   hop_stdout = g_io_channel_unix_new( hop_pipe_stdout[ 0 ] );
   hop_stderr = g_io_channel_unix_new( hop_pipe_stderr[ 0 ] );

   g_io_add_watch( hop_stdout, G_IO_IN | G_IO_HUP, watch_output, 0 );
   g_io_add_watch( hop_stderr, G_IO_IN | G_IO_HUP, watch_output, 0 );
   
   g_io_channel_set_flags( hop_stdout, G_IO_FLAG_NONBLOCK, &err );
   g_io_channel_set_flags( hop_stderr, G_IO_FLAG_NONBLOCK, &err );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    launch_error ...                                                 */
/*---------------------------------------------------------------------*/
void
launch_error( char* msg, char *err ) {
   fprintf( stderr, "*** ERROR: hop-launcher -- %s\n%s\n...exiting.\n",
	    msg, err );
   exit( 1 );
}

/*---------------------------------------------------------------------*/
/*    static pid_t                                                     */
/*    exec_hop ...                                                     */
/*    -------------------------------------------------------------    */
/*    This function starts a hop in background. If there is already    */
/*    one running, we kill it first.                                   */
/*---------------------------------------------------------------------*/
static pid_t
exec_hop( char *msg ) {
   int pid;

   /* show the waiting widget */
   gui_start_waiting( msg );
   
   if( (pipe( hop_pipe_stdout ) < 0) || (pipe( hop_pipe_stderr ) < 0) ) {
      launch_error( "Cannot create pipe", strerror( errno ) );
   }
   
   switch( pid = fork() ) {
      case -1:
	 launch_error( "Cannot fork", strerror( errno ) );
	 return -1;

      case 0:
	 /* redirect to the pipe */
	 if( dup2( hop_pipe_stdout[ 1 ], 1 ) < -1 ) {
	    launch_error( "Cannot duplicate stdout", strerror( errno ) );
	 }
	 if( dup2( hop_pipe_stderr[ 1 ], 2 ) < -1 ) {
	    launch_error( "Cannot duplicate stderr", strerror( errno ) );
	 }

	 /* exec the hop process */
	 if( exec( get_hop_command( hop_command ) ) == -1 ) {
	    launch_error( "Cannot start hop", strerror( errno ) );
	 }
	 return -1;

      default:
	 /* the parent process */
	 return pid;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    kill_hop ...                                                     */
/*---------------------------------------------------------------------*/
void
kill_hop() {
   if( hop_pid ) {
      int info;
      
      close_io_channels();

      if( hop_kill_command )
	 system( get_hop_command( hop_kill_command ) );
      else 
	 kill( hop_pid, SIGKILL );
      
      waitpid( hop_pid, &info, 0 );
      hop_pid = 0;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    run_hop ...                                                      */
/*---------------------------------------------------------------------*/
void
run_hop() {
   if( hop_pid ) {
      kill_hop();
      
      /* an instance of Hop is already running */
      hop_pid = exec_hop( "Restarting Hop" );
      init_io_channels();
   } else {
      /* start our first Hop */
      hop_pid = exec_hop( "Waiting for Hop" );
      init_io_channels();
   }
}
