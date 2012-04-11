/*=====================================================================*/
/*    serrano/prgm/project/hop/linux/maemo/hop-launcher/param.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Dec 24 10:59:37 2007                          */
/*    Last change :  Sat Aug  2 07:53:51 2008 (serrano)                */
/*    Copyright   :  2007-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hop-launcher parameters module                                   */
/*=====================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include "config.h"
#include "param.h"
#include "list.h"
#include "read.h"

/*---------------------------------------------------------------------*/
/*    Global parameters                                                */
/*---------------------------------------------------------------------*/
char *hop_port = HOP_DEFAULT_PORT;
char *hop_command = HOP_DEFAULT_CMD;
char *hop_kill_command = 0L;

#define MKDIR_MODE 511

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    hop_read_param ...                                               */
/*---------------------------------------------------------------------*/
void
hop_read_param() {
   char fname[ 256 ];

   sprintf( fname, "%s/.config/hop/hop-launcher", getenv( "HOME" ) );
   
   if( !access( fname, R_OK ) ) {
      FILE *fin = fopen( fname, "r" );
      obj_t *obj = readobj( fin );

      fclose( fin );

      while( PAIRP( obj ) ) {
	 if( PAIRP( CAR( obj ) ) && \
	     SYMBOLP( CAR( CAR( obj ) ) ) && \
	     STRINGP( CADR( CAR( obj ) ) ) ) {
	    char *sym = SYMBOL_CHARS( CAR( CAR( obj ) ) );

	    if( !strcmp( sym, "port" ) )
	       hop_port = STRING_CHARS( CADR( CAR( obj ) ) );
	    if( !strcmp( sym, "command" ) )
	       hop_command = STRING_CHARS( CADR( CAR( obj ) ) );
	    if( !strcmp( sym, "kill" ) )
	       hop_kill_command = STRING_CHARS( CADR( CAR( obj ) ) );
	 }

	 obj = (obj_t *)CDR( obj );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    directoryp ...                                                   */
/*    -------------------------------------------------------------    */
/*    Is a file a directory?                                           */
/*---------------------------------------------------------------------*/
int
directoryp( char *name ) { 
   struct stat buf;

   if( stat( name, &buf ) == -1 )
      return 0;

   return S_ISDIR( buf.st_mode & S_IFMT );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    hop_write_param ...                                              */
/*---------------------------------------------------------------------*/
void
hop_write_param() {
   char fname[ 256 ];

   sprintf( fname, "%s/.config", getenv( "HOME" ) );
   mkdir( fname, MKDIR_MODE );

   strcat( fname, "hop" );
   mkdir( fname, MKDIR_MODE );

   if( directoryp( fname ) ) {
      FILE *fout;
      
      strcat( fname, "hop-launcher" );

      if( (fout = fopen( fname, "w" )) ) {
	 fputs( "(\n", fout );
	 fprintf( fout, " (port \"%s\")\n", hop_port );
	 fprintf( fout, " (command \"%s\")\n", hop_command );
	 fprintf( fout, " (kill \"%s\")\n", hop_kill_command );
	 fputs( ")\n", fout );
	 fclose( fout );
      }      
   }
}

