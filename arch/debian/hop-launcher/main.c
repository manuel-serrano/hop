/*=====================================================================*/
/*    serrano/prgm/project/hop/linux/maemo/hop-launcher/main.c         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 14 07:35:47 2007                          */
/*    Last change :  Sun Nov 29 20:11:32 2009 (serrano)                */
/*    Copyright   :  2007-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Hildon application to launch Hop on Maemo (Nokia N8X0).      */
/*=====================================================================*/
#include "config.h"
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#if( defined( MAEMO4 ) || defined( MAEMO5 ) )
#  include <hildon/hildon-program.h>
#  include <hildon/hildon-banner.h>
#else
#  include <hildon-widgets/hildon-program.h>
#  include <hildon-widgets/hildon-banner.h>
#endif
#include <gtk/gtk.h>
#include <gtk/gtkmain.h>
#include <glib.h>
#include <libosso.h>
#include "gui.h"
#include "hop.h"
#include "param.h"

/*---------------------------------------------------------------------*/
/*    OSSO (DBUS) variables and constants                              */
/*---------------------------------------------------------------------*/
#define OSSO_HOP_NAME    "hop"
#define OSSO_HOP_SERVICE "fr.inria."OSSO_HOP_NAME
#define OSSO_HOP_OBJECT  "/fr/inria/"OSSO_HOP_NAME
#define OSSO_HOP_IFACE   "fr.inria."OSSO_HOP_NAME

static osso_context_t *osso_context;

/* Application UI data struct */
typedef struct _AppData {
   HildonProgram *program;
   HildonWindow *window;
   osso_context_t *osso_context;
} AppData;

/*---------------------------------------------------------------------*/
/*    gint                                                             */
/*    dbus_req_handler ...                                             */
/*---------------------------------------------------------------------*/
gint
dbus_req_handler( const gchar *interface, const gchar *method,
		  GArray *arguments, gpointer data, osso_rpc_t *retval ) {
    AppData *appdata;
    appdata = (AppData *)data;

    /* osso_system_note_infoprint( appdata->osso_context, method, retval ); */
    osso_rpc_free_val( retval );

    return OSSO_OK;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    cleanup ...                                                      */
/*---------------------------------------------------------------------*/
void cleanup() {
   kill_hop();
   osso_deinitialize( osso_context );
   gtk_main_quit();
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    osso_init ...                                                    */
/*---------------------------------------------------------------------*/
int
osso_init() {
   osso_return_t result;
   AppData *appdata;
   
   /* Initialize maemo application */
   osso_context = osso_initialize( "hop_launcher", HOP_RELEASE, TRUE, NULL );

   /* Check that initialization was ok */
   if( osso_context == NULL ) {
      return OSSO_ERROR;
   }
   
   /* Create AppData */
   appdata = g_new0( AppData, 1 );
   appdata->program = program;
   appdata->window = win;
   appdata->osso_context = osso_context;

   /* Add handler for session bus D-BUS messages */
   result = osso_rpc_set_cb_f( appdata->osso_context, 
                               OSSO_HOP_SERVICE, 
                               OSSO_HOP_OBJECT, 
                               OSSO_HOP_IFACE,
                               dbus_req_handler, appdata );

   if( result != OSSO_OK ) {
      g_print( "Error setting D-BUS callback (%d)\n", result );
      return OSSO_ERROR;
   }

   return OSSO_OK;
}
   

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    main ...                                                         */
/*---------------------------------------------------------------------*/
int
main( int argc, char *argv[] ) {
   /* Initialize the GTK. */
   gtk_init( &argc, &argv );

   /* read the user configuration */
   hop_read_param();
   
   /* Create the main GUI */
   make_hop_gui();

   /* Initialize dbus */
   osso_init();
   
   /* Prepare the data structure for subprocess I/O */
   init_hop_io();

   /* Add signal listener to button */
   run_hop();

   /* Begin the main application */
   gtk_widget_show_all( GTK_WIDGET( win ) );

   /* Connect signal to X in the upper corner */
   g_signal_connect( G_OBJECT( win ), "delete_event",
		     G_CALLBACK( cleanup ), NULL );

   gtk_main();

   /* We are done */
   kill_hop();
   osso_deinitialize( osso_context );
   
   /* Exit */
   return 0;
}
