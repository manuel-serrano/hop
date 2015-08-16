/*=====================================================================*/
/*    serrano/prgm/project/hop/linux/maemo/hop-launcher/gui.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Dec 16 07:58:09 2007                          */
/*    Last change :  Sun Nov 29 20:12:14 2009 (serrano)                */
/*    Copyright   :  2007-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Functions that deal with the GUI.                                */
/*=====================================================================*/
#include "config.h"
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#if( defined( MAEMO4 ) || defined( MAEMO5 ) )
#  include <hildon/hildon-program.h>
#  include <hildon/hildon-banner.h>
#else
#  include <hildon-widgets/hildon-program.h>
#  include <hildon-widgets/hildon-banner.h>
#  include <gtk/gtk.h>
#endif
#include <gtk/gtkmain.h>
#include <glib.h>
#include "hop.h"
#include "gui.h"

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
extern pid_t launch_hop();

/*---------------------------------------------------------------------*/
/*    Global variables                                                 */
/*---------------------------------------------------------------------*/
HildonProgram *program;
HildonWindow *win;
static GtkWidget* console;
static GtkWidget *waiting_banner = 0L;
static GtkWidget *main_hpan;
static GtkWidget *left_vbox;
static GtkWidget *right_vbox;
static GtkWidget *waiting_banner;
static GtkTextBuffer *console_buffer;
static GtkTextIter end_of_buffer;
static GtkWidget *port_entry;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    make_main_gui ...                                                */
/*---------------------------------------------------------------------*/
static void
make_main_gui() {
   main_hpan = gtk_hpaned_new();
   left_vbox = gtk_vbox_new( FALSE, 0 );
   right_vbox = gtk_vbox_new( FALSE, 0 );
   
   gtk_container_add( GTK_CONTAINER( main_hpan ), left_vbox );
   gtk_container_add( GTK_CONTAINER( main_hpan ), right_vbox );
   gtk_container_add( GTK_CONTAINER( win ), main_hpan );
}

/*---------------------------------------------------------------------*/
/*    GtkWidget *                                                      */
/*    make_button ...                                                  */
/*---------------------------------------------------------------------*/
GtkWidget *
make_button( char *msg ) {
   static PangoFontDescription *font_desc = 0;
   GtkWidget *but = gtk_button_new();
   GtkWidget *lbl = gtk_label_new( msg );

   if( !font_desc )
      font_desc =  pango_font_description_from_string( HOP_BUTTON_FONT );
   
   gtk_widget_modify_font( lbl, font_desc );
   
   gtk_container_add( GTK_CONTAINER( but ), lbl );
   
   return but;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    make_hop_console ...                                             */
/*---------------------------------------------------------------------*/
static void
make_hop_console() {
   GtkWidget *view;
   GtkWidget *but;
   GtkWidget *hboxbut;
   PangoFontDescription *font_desc;

   console = gtk_scrolled_window_new( NULL, NULL );
   view = gtk_text_view_new();
   
   console_buffer = gtk_text_view_get_buffer( GTK_TEXT_VIEW( view ) );
   gtk_text_buffer_get_iter_at_offset( console_buffer, &end_of_buffer, -1 );
   
   font_desc = pango_font_description_from_string( HOP_CONSOLE_FONT );
   
   gtk_widget_modify_font( view, font_desc );
   pango_font_description_free( font_desc );
   
   gtk_container_add( GTK_CONTAINER( console ), view );
   gtk_container_add( GTK_CONTAINER( right_vbox ), console );
   
   hboxbut = gtk_hbutton_box_new();
   but = make_button( "Clear console" );
   g_signal_connect( G_OBJECT( but ), "clicked",
		     G_CALLBACK( clear_hop_console ), console );
   gtk_container_add( GTK_CONTAINER( hboxbut ), but );
   gtk_box_pack_end( GTK_BOX( right_vbox ), hboxbut, FALSE, FALSE, 2 );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    clear_hop_console ...                                            */
/*---------------------------------------------------------------------*/
void
clear_hop_console() {
   GtkTextIter start_del;
   GtkTextIter end_del;

   gtk_text_buffer_get_iter_at_offset( console_buffer, &start_del, 0 );
   gtk_text_buffer_get_iter_at_offset( console_buffer, &end_del, -1 );
      
   gtk_text_buffer_delete( console_buffer, &start_del, &end_del );
   gtk_text_buffer_get_iter_at_offset( console_buffer, &end_of_buffer, -1 );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    hop_console_insert ...                                           */
/*---------------------------------------------------------------------*/
void
hop_console_insert( char *buf, int sz ) {
   gtk_text_buffer_insert( console_buffer, &end_of_buffer, buf, sz );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    hop_console_adjust ...                                           */
/*---------------------------------------------------------------------*/
void
hop_console_adjust() {
   GtkAdjustment *adj = gtk_scrolled_window_get_vadjustment( GTK_SCROLLED_WINDOW( console ) );
   int count = gtk_text_buffer_get_line_count( console_buffer );

   if( count > HOP_CONSOLE_MAX_LINE ) {
      GtkTextIter start_del;
      GtkTextIter end_del;
      int l = count - HOP_CONSOLE_MAX_LINE;

      gtk_text_buffer_get_iter_at_offset( console_buffer, &start_del, 0 );
      gtk_text_buffer_get_iter_at_line_offset( console_buffer, &end_del, l, 0 );
      
      gtk_text_buffer_delete( console_buffer, &start_del, &end_del );
   }
   
/*    gtk_adjustment_set_value( adj, adj->upper );                     */
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    make_hop_logo ...                                                */
/*---------------------------------------------------------------------*/
static void
make_hop_logo() {
   PangoFontDescription *version_font_desc;
   PangoFontDescription *url_font_desc;
   GtkWidget *version_lbl, *url_lbl;
   GtkWidget *hostname_lbl, *ip_lbl;
   struct hostent *hp;
   static char hostname[ MAXHOSTNAME ];
   char *ip;

   gethostname( hostname, MAXHOSTNAME );
   if( (hp = gethostbyname( "127.0.0.1" )) ) {
      char **runner = hp->h_addr_list;

      if( *runner && runner++ && *runner ) {
	 ip = inet_ntoa( *(struct in_addr *)(*runner) );
      } else {
	 ip = "127.0.0.1";
      }
   } else {
      ip = "0.0.0.0";
   }
   
   version_lbl = gtk_label_new( "Hop v" HOP_RELEASE );
   url_lbl = gtk_label_new( HOP_URL );
   hostname_lbl = gtk_label_new( hostname );
   ip_lbl = gtk_label_new( ip );
   
   version_font_desc = pango_font_description_from_string( HOP_VERSION_FONT );
   url_font_desc = pango_font_description_from_string( HOP_URL_FONT );
   gtk_widget_modify_font( version_lbl, version_font_desc );
   gtk_widget_modify_font( url_lbl, url_font_desc );
   gtk_widget_modify_font( hostname_lbl, url_font_desc );
   gtk_widget_modify_font( ip_lbl, url_font_desc );
   
   pango_font_description_free( version_font_desc );
   pango_font_description_free( url_font_desc );

   gtk_box_pack_start( GTK_BOX( left_vbox ),
		       gtk_image_new_from_file( HOP_LOGO ),
		       FALSE, FALSE, 2 );
   gtk_box_pack_start( GTK_BOX( left_vbox ),
		       GTK_WIDGET( version_lbl ),
		       FALSE, FALSE, 2 );
   gtk_box_pack_start( GTK_BOX( left_vbox ),
		       GTK_WIDGET( url_lbl ),
		       FALSE, FALSE, 2 );
   gtk_box_pack_start( GTK_BOX( left_vbox ),
		       GTK_WIDGET( gtk_hseparator_new() ),
		       FALSE, FALSE, 2 );
   gtk_box_pack_start( GTK_BOX( left_vbox ),
		       GTK_WIDGET( hostname_lbl ),
		       FALSE, FALSE, 2 );
   gtk_box_pack_start( GTK_BOX( left_vbox ),
		       GTK_WIDGET( ip_lbl ),
		       FALSE, FALSE, 2 );
   gtk_box_pack_start( GTK_BOX( left_vbox ),
		       GTK_WIDGET( gtk_hseparator_new() ),
		       FALSE, FALSE, 2 );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    run_hop_with_port ...                                            */
/*---------------------------------------------------------------------*/
void
run_hop_with_port() {
   hop_port = (char *)gtk_entry_get_text( GTK_ENTRY( port_entry ) );
   run_hop();
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    make_hop_restart ...                                             */
/*---------------------------------------------------------------------*/
void
make_hop_restart() {
   PangoFontDescription *font_desc;
   GtkWidget *port_lbl;
   GtkWidget *hbox, *hboxbut;
   GtkWidget *but;
   
   font_desc = pango_font_description_from_string( HOP_MONOSPACE_FONT );
   port_lbl = gtk_label_new( "Port: " );
   port_entry = gtk_entry_new();
   gtk_entry_set_width_chars( GTK_ENTRY( port_entry ), 4 );
   hbox = gtk_hbox_new( FALSE, 0 );

   gtk_container_add( GTK_CONTAINER( hbox ), GTK_WIDGET( port_lbl ) );
   gtk_container_add( GTK_CONTAINER( hbox ), GTK_WIDGET( port_entry ) );
   gtk_entry_set_text( GTK_ENTRY( port_entry ), (const gchar *)hop_port );
   gtk_widget_modify_font( port_lbl, font_desc );
   gtk_widget_modify_font( port_entry, font_desc );
   
   pango_font_description_free( font_desc );

   but = make_button( "Restart Hop" );
   g_signal_connect( G_OBJECT( but ), "clicked",
		     G_CALLBACK( run_hop_with_port ), win );

   hboxbut = gtk_hbutton_box_new();
   gtk_box_pack_start( GTK_BOX( hboxbut ), but, FALSE, FALSE, 2 );
   
   gtk_box_pack_start( GTK_BOX( left_vbox ), hboxbut, FALSE, FALSE, 10 );
   gtk_box_pack_start( GTK_BOX( left_vbox ), hbox, FALSE, FALSE, 2 );
}

/*---------------------------------------------------------------------*/
/*    HildonWindow *                                                   */
/*    make_hop_gui ...                                                 */
/*---------------------------------------------------------------------*/
HildonWindow *
make_hop_gui() {
   /* Create the Hildon program and setup the title */
   program = HILDON_PROGRAM( hildon_program_get_instance() );
   g_set_application_name( "Hop launcher" );

   /* Create HildonWindow and set it to HildonProgram */
   win = HILDON_WINDOW( hildon_window_new() );
   hildon_program_add_window( program, win );

   /* Add vbox to hildon window */
   make_main_gui();
   
   /* The Hop logo and version */
   make_hop_logo();
   
   /* The console */
   make_hop_console();

   /* The restart HOP area */
   make_hop_restart();

   return win;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    gui_start_waiting ...                                            */
/*---------------------------------------------------------------------*/
void
gui_start_waiting( char *msg ) {
   if( !waiting_banner ) {
      waiting_banner =
	 hildon_banner_show_animation( GTK_WIDGET( win ), NULL, msg );
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    gui_stop_waiting ...                                             */
/*---------------------------------------------------------------------*/
void
gui_stop_waiting() {
   if( waiting_banner ) {
      gtk_widget_destroy( GTK_WIDGET( waiting_banner ) );
      waiting_banner = 0L;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    gui_message ...                                                  */
/*---------------------------------------------------------------------*/
void
gui_message( char *msg ) {
   hildon_banner_show_information( GTK_WIDGET( win ), NULL, msg );
}
