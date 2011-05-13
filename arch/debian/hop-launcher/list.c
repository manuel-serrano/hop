/*=====================================================================*/
/*    serrano/prgm/project/hop/linux/maemo/hop-launcher/list.c         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Oct 15 07:04:35 2003                          */
/*    Last change :  Mon Dec 24 10:58:42 2007 (serrano)                */
/*    Copyright   :  2003-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    List toolkit                                                     */
/*=====================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "list.h"
 
/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    type_name ...                                                    */
/*---------------------------------------------------------------------*/
char *
type_name( obj_t * o ) {
   switch( o->type ) {
      case TYPE_PAIR: return "pair";
      case TYPE_SYMBOL: return "symbol";
      case TYPE_STRING: return "string";
      case TYPE_INTEGER: return "integer";
      default: return "???";
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    display_list ...                                                 */
/*---------------------------------------------------------------------*/
void
display_list( FILE *fout, pair_t *o ) {
   fprintf( fout, "(" );

   do {
      display( fout, (obj_t *)(CAR( o ) ) );
      if( PAIRP( CDR( o ) ) ) {
	 fprintf( fout, " " );
	 o = CDR( o );
      } else {
	 o = NIL;
      }
   } while( PAIRP( o ) );
   
   fprintf( fout, ")" );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    display ...                                                      */
/*---------------------------------------------------------------------*/
void
display( void *fout, obj_t *o ) {
   if( !o ) {
      fprintf( (FILE *)fout, "()" );
      return;
   } else {
      switch( o->type ) {
	 case TYPE_PAIR:
	    display_list( (FILE *)fout, (pair_t *)o );
	    return;
	 
	 case TYPE_SYMBOL:
	    fprintf( (FILE *)fout, "%s", SYMBOL_CHARS( o ) );
	    return;
	 
	 case TYPE_STRING:
	    fprintf( (FILE *)fout, "\"%s\"", STRING_CHARS( o ) );
	    return;
	 
	 case TYPE_INTEGER:
	    fprintf( (FILE *)fout, "%ld", INTEGER_VAL( o ) );
	    return;
	 
	 default:
	    fprintf( (FILE *)fout, "<???:%p>", o );
	    return;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    pair_t *                                                         */
/*    cons ...                                                         */
/*---------------------------------------------------------------------*/
pair_t *
cons( void *a, pair_t *d ) {
   pair_t *res = (pair_t *)malloc( sizeof( pair_t ) );
   
   res->type = TYPE_PAIR;
   SET_CAR( res, a );
   SET_CDR( res, d );

   return res;
}

/*---------------------------------------------------------------------*/
/*    pair_t *                                                         */
/*    memq ...                                                         */
/*---------------------------------------------------------------------*/
pair_t *
memq( void *o, pair_t *l ) {
   while( PAIRP( l ) ) {
      if( CAR( l ) == o )
	 return l;
      l = CDR( l );
   }
   return NIL;
}

/*---------------------------------------------------------------------*/
/*    pair_t *                                                         */
/*    remq ...                                                         */
/*---------------------------------------------------------------------*/
pair_t *
remq( void *o, pair_t *l ) {
   if( NULLP( l ) )
      return l;
	 
   if( CAR( l ) == o ) {
      pair_t *res = CDR( l );
      free( l );
      return res;
   } else {
      pair_t *s = l;
      
      while( PAIRP( CDR( l ) ) ) {
	 if( CAR( CDR( l ) ) == o ) {
	    pair_t *f = CDR( l );
	    
	    SET_CDR( l, CDR( CDR( l ) ) );
	    free( f );
	    
	    break;
	 }

	 l = CDR( l );
      }
	    
      return s;
   }
}

/*---------------------------------------------------------------------*/
/*    pair_t *                                                         */
/*    reverse ...                                                      */
/*---------------------------------------------------------------------*/
pair_t *
reverse( pair_t *l ) {
   if( !PAIRP( l ) ) {
      return l;
   } else {
      pair_t *r = NIL;

      while( !NULLP( CDR( l ) ) ) {
	 pair_t *cdrl = CDR( l );
	 
	 SET_CDR( l, r );
	 r = l;
	 l = cdrl;
      }
      SET_CDR( l, r );
      return l;
   }
}
      
/*---------------------------------------------------------------------*/
/*    pair_t *                                                         */
/*    last_pair ...                                                    */
/*---------------------------------------------------------------------*/
pair_t *
last_pair( pair_t *lst ) {
   if( PAIRP( lst ) ) {
      pair_t *lst2;
      
      while( PAIRP( lst2 = (pair_t *)CDR( lst ) ) ) lst = lst2;
      
      return lst;
   } else {
      return lst;
   }
}

/*---------------------------------------------------------------------*/
/*    string_t *                                                       */
/*    make_symbol ...                                                  */
/*---------------------------------------------------------------------*/
symbol_t *
make_symbol( char *s ) {
   static pair_t *symlist = NIL;
   pair_t *lst = symlist;
   symbol_t *res;

   while( PAIRP( lst ) ) {
      symbol_t *car = (symbol_t *)CAR( lst );
      if( !strcmp( s, car->chars ) ) {
	 return car;
      } else {
	 lst = CDR( lst );
      }
   }
   
   res = (symbol_t *)malloc( sizeof( symbol_t ) + strlen( s ) );
   res->type = TYPE_SYMBOL;
   strcpy( &(res->chars[ 0 ]), s );

   symlist = cons( res, symlist );

   return res;
}

/*---------------------------------------------------------------------*/
/*    string_t *                                                       */
/*    make_string ...                                                  */
/*---------------------------------------------------------------------*/
string_t *
make_string( char *s ) {
   string_t *res = (string_t *)malloc( sizeof( string_t ) + strlen( s ) );

   res->type = TYPE_STRING;
   strcpy( &(res->chars[ 0 ]), s );

   return res;
}

/*---------------------------------------------------------------------*/
/*    integer_t *                                                      */
/*    make_integer ...                                                 */
/*---------------------------------------------------------------------*/
integer_t *
make_integer( long v ) {
   integer_t *res = (integer_t *)malloc( sizeof( integer_t ) );

   res->type = TYPE_INTEGER;
   res->val = v;

   return res;
}
   
