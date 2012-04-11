/*=====================================================================*/
/*    serrano/prgm/project/hop/linux/maemo/hop-launcher/read.c         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Aug  1 05:55:00 2004                          */
/*    Last change :  Wed Jan 16 08:35:44 2008 (serrano)                */
/*    Copyright   :  2004-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    parsing                                                          */
/*=====================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "list.h"
#include "read.h"

/*---------------------------------------------------------------------*/
/*    Tokens                                                           */
/*---------------------------------------------------------------------*/
#define TOKEN_NONE    1
#define TOKEN_SYMBOL  2
#define TOKEN_STRING  3
#define TOKEN_INT     4
#define TOKEN_OPENPAR '('
#define TOKEN_CLOPAR  ')'
#define TOKEN_GUIL    '"'

/*---------------------------------------------------------------------*/
/*    token_t                                                          */
/*---------------------------------------------------------------------*/
typedef struct token {
   int tok;
   char *val;
} token_t;

/*---------------------------------------------------------------------*/
/*    token_t *                                                        */
/*    make_token ...                                                   */
/*---------------------------------------------------------------------*/
token_t *
make_token( int tok, char *val ) {
   token_t *tk = calloc( 1, sizeof( token_t ) );

   tk->tok = tok;

   if( val ) {
      tk->val = malloc( strlen( val ) + 1 );
      strcpy( tk->val, val );
   }
   
   return tk;
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    token_type ...                                                   */
/*---------------------------------------------------------------------*/
static char *
token_type( token_t *tok ) {
   if( !tok ) {
      return "???";
   }
   
   switch( tok->tok ) {
      case TOKEN_NONE:
	 return "???";

      case TOKEN_SYMBOL:
	 return "symbol";

      case TOKEN_STRING:
	 return "string";

      case TOKEN_INT:
	 return "int";

      case TOKEN_OPENPAR:
	 return "open parenthesis";
	 
      case TOKEN_CLOPAR:
	 return "close parenthesis";

      case TOKEN_GUIL:
	 return "guillemet";

      default:
	 return "???";
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    display_token ...                                                */
/*---------------------------------------------------------------------*/
/*
static void
display_token( token_t *tok ) {
   if( !tok ) {
      printf( "???" );
   } else {
      switch( tok->tok ) {
	 case TOKEN_NONE:
	    return;

	 case TOKEN_SYMBOL:
	 case TOKEN_STRING:
	 case TOKEN_INT:
	    fprintf( stderr, "%s", tok->val );
	    return;

	 case TOKEN_OPENPAR:
	    fprintf( stderr, "(" );
	    return;
	 
	 case TOKEN_CLOPAR:
	    fprintf( stderr, ")" );
	    return;
	 
	 case TOKEN_GUIL:
	    fprintf( stderr, "\"" );
	    return;
	 
	 default:
	    fprintf( stderr, "???" );
	    return;
      }
   }
}
*/

/*---------------------------------------------------------------------*/
/*    token_t *                                                        */
/*    parse_token ...                                                  */
/*---------------------------------------------------------------------*/
token_t *
parse_token( FILE *file ) {
   static char buffer[ 1024 ];
   static int last = 0;
   int c, i = 0;
   int tok = TOKEN_NONE;

   c = last ? last : fgetc( file );

   while( c != EOF ) {
      switch( c ) {
	 case '(':
	 case ')':
	 case '"':
	    if( i == 0 ) {
	       last = 0;
	       return make_token( c, 0L );
	    } else {
	       goto _end;
	    }

	 case ';':
	    if( i == 0 ) {
	       do {
		  c = fgetc( file );
	       } while( (c != EOF) && (c != '\n') );
	       last = 0;
	       goto _next;
	    } else {
	       goto _end;
	    }
	    
	 case ' ':
	 case '\t':
	 case '\n':
	    last = 0;
	    if( i == 0 )
	       goto _next;
	    else
	       goto _end;

	 case '0':
	 case '1':
	 case '2':
	 case '3':
	 case '4':
	 case '5':
	 case '6':
	 case '7':
	 case '8':
	 case '9':
	    if( i == 0 ) tok = TOKEN_INT;
	    break;

	 case '+':
	 case '-':
	    tok = ( i == 0 ) ? TOKEN_INT : TOKEN_SYMBOL;
	    break;
	    
	 default:
	    tok = TOKEN_SYMBOL;
	    
      }
      
      buffer[ i++ ] = c;
_next:
      c = fgetc( file );
   }

_end:
   last = c;
   
   if( tok != TOKEN_NONE ) {
      buffer[ i ] = 0;
      return make_token( tok, buffer );
   } else {
      return 0L;
   }
}

/*---------------------------------------------------------------------*/
/*    string_t *                                                       */
/*    readstring ...                                                   */
/*---------------------------------------------------------------------*/
string_t *
readstring( FILE *file ) {
   token_t *tok = parse_token( file );

   if( !tok ) {
      fprintf( stderr, "Premature end of file\n" );
      return 0L;
   } else {
      if( tok->tok == TOKEN_SYMBOL ) {
	 token_t *tok2 = parse_token( file );

	 if( !tok2 ) {
	    fprintf( stderr, "Premature end of file\n" );
	    return 0L;
	 } else {
	    if( tok2->tok == TOKEN_GUIL ) {
	       return make_string( tok->val );
	    } else {
	       fprintf( stderr, "Illegal string...%s\n", tok->val );
	       return 0L;
	    }
	 }
      } else {
	 return 0L;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    string_t *                                                       */
/*    readguil ...                                                     */
/*---------------------------------------------------------------------*/
string_t *
readguil( FILE *file ) {
   static char buffer[ 1024 ];
   int c, i = 0;

   c = fgetc( file );
   
   while( c != EOF ) {
      if( c == '"' ) {
	 buffer[ i ] = 0;
	 return make_string( buffer );
      } else {
	 buffer[ i++ ] = c;
	 c = fgetc( file );
      }
   }

   fprintf( stderr, "Premature end of file\n" );
   return 0L;
}

/*---------------------------------------------------------------------*/
/*    pair_t *                                                         */
/*    readlist ...                                                     */
/*---------------------------------------------------------------------*/
pair_t *
readlist( FILE *file ) {
   token_t *tok = parse_token( file );
   pair_t *res = NIL;

   if( !tok ) {
      fprintf( stderr, "Premature end of file\n" );
      return 0L;
   } else {
      if( tok->tok == TOKEN_CLOPAR ) {
	 return res;
      } else {
	 obj_t *car;
	 switch( tok->tok ) {
	    case TOKEN_OPENPAR:
	       car = (obj_t *)readlist( file );
	       break;
	    
	    case TOKEN_SYMBOL:
	       car = (obj_t *)make_symbol( tok->val );
	       break;
	    
	    case TOKEN_GUIL:
	       car = (obj_t *)readguil( file );
	       if( !car ) car = (obj_t *)NIL;
	       break;
	    
	    case TOKEN_INT:
	       car = (obj_t *)make_integer( atol( tok->val ) );
	       break;
	    
	    default:
	       fprintf( stderr, "Illegal %s: %s\n",
			token_type( tok ),
			tok->val );
	       car = (obj_t *)NIL;
	 } 
	 
	 return cons( car, readlist( file ) );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t *                                                          */
/*    readobj ...                                                      */
/*---------------------------------------------------------------------*/
obj_t *
readobj( FILE *file ) {
   token_t *tok = parse_token( file );

   if( !tok ) {
      return 0L;
   } else {
      switch( tok->tok ) {
	 case TOKEN_OPENPAR:
	    return (obj_t *)readlist( file );
	    
	 case TOKEN_SYMBOL:
	    return (obj_t *)make_symbol( tok->val );
	    
	 case TOKEN_STRING:
	    return (obj_t *)make_string( tok->val );
	    
	 case TOKEN_INT:
	    return (obj_t *)make_integer( atol( tok->val ) );
	    
	 default:
	    fprintf( stderr, "Illegal %s: %s\n",
		     token_type( tok ),
		     tok->val );
	    return (obj_t *)NIL;
      }
   }
}
