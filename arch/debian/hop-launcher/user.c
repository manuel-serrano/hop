/*=====================================================================*/
/*    serrano/prgm/project/hop/linux/maemo/hop-launcher/user.c         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Dec 17 11:27:21 2007                          */
/*    Last change :  Sun Dec 23 18:41:48 2007 (serrano)                */
/*    Copyright   :  2007 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    User management                                                  */
/*=====================================================================*/
#include "config.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "user.h"

/*---------------------------------------------------------------------*/
/*    Global declarations                                              */
/*---------------------------------------------------------------------*/
user_t *current_user;
user_t *hop_user;
user_t *selected_user = 0;

static char *hop_default_user_name = HOP_USER;

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    readline ...                                                     */
/*---------------------------------------------------------------------*/
int
readline( FILE *stream, char *buf, long size ) {
   char *ptr = buf;
   long num = size;
   int c;

   while( ((c = getc( stream )) != EOF) ) {
      *buf++ = c;

      if( c == '\n' ) break;
      if( --num <= 0 ) break;
   }

   return (long)(buf - ptr);
}
  
/*---------------------------------------------------------------------*/
/*    uid_t                                                            */
/*    find_user_uid ...                                                */
/*---------------------------------------------------------------------*/
uid_t
find_user_uid( char *name ) {
   char buf[ 1025 ];
   FILE *fin = fopen( "/etc/passwd", "r" );
   int len = strlen( name );
   int n;
   long uid = 0;

   if( !fin ) return 0;

   while( (n = readline( fin, buf, 1024 )) > 0 ) {
      buf[ n ] = 0;

      if( (n > len) && (buf[ len ] == ':') && (!strncmp( buf, name, len )) ) {
	 char *s;

	 /* got it */
	 if( strtok( buf, ":" ) &&
	     strtok( 0, ":" ) &&
	     (s = strtok( 0, ":" )) ) {
	    sscanf( s, "%ld", &uid );
	 }
	 goto _ret;
      }
   }

_ret:
   fclose( fin );

   return uid;
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    find_user_name ...                                               */
/*---------------------------------------------------------------------*/
char *
find_user_name( int uid ) {
   char buf[ 1025 ];
   FILE *fin = fopen( "/etc/passwd", "r" );
   int n;
   long suid = 0;
   char *res = 0;

   if( !fin ) return 0;

   while( (n = readline( fin, buf, 1024 )) > 0 ) {
      buf[ n ] = 0;
      char *s, *tmp;

      tmp = strtok( buf, ":" );
      
      if( tmp && strtok( 0, ":" ) && (s = strtok( 0, ":" )) ) {
	 sscanf( s, "%ld", &suid );

	 if( uid == suid ) {
	    res = malloc( strlen( tmp + 1 ) );
	    strcpy( res, tmp );

	    goto _ret;
	 }
      }
   }

_ret:
   fclose( fin );

   return res;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    init_users ...                                                   */
/*---------------------------------------------------------------------*/
void
init_users() {
   char buffer[ 512 ];
   current_user = malloc( sizeof( user_t ) );

   current_user->uid = getuid();
   current_user->name = find_user_name( current_user->uid );

   sprintf( buffer, "%s/.config/hop.user", getenv( "HOME" ) );
   
   if( !access( buffer, R_OK ) ) {
      FILE *fin = fopen( buffer, "r" );

      if( fin ) {
	 char *buf = malloc( 255 );
	 int n;

	 n = readline( fin, buf, 80 );

	 if( n > 0 ) {
	    if( buf[ n - 1 ] == '\n' ) {
	       buf[ n - 1 ] = 0;
	    } else {
	       buf[ n ] = 0;
	    }
	    hop_default_user_name = buf;
	 } else {
	    free( buf );
	 }
	 
	 fclose( fin );
      }
   }
   
   if( !strcmp( current_user->name, hop_default_user_name ) ) {
      hop_user = current_user;
   } else {
      uid_t uid = find_user_uid( hop_default_user_name );

      if( uid ) {
	 hop_user = malloc( sizeof( user_t ) );
	 
	 hop_user->uid = uid;
	 hop_user->name = hop_default_user_name;
	 selected_user = hop_user;
      } else {
	 hop_user = current_user;
      }
   }
}

