/*=====================================================================*/
/*    serrano/prgm/project/hop/linux/maemo/hop-launcher/list.h         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Oct 15 07:04:04 2003                          */
/*    Last change :  Mon Dec 24 10:58:27 2007 (serrano)                */
/*    Copyright   :  2003-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    List toolkit                                                     */
/*=====================================================================*/
#ifndef _HOP_LAUNCHER_LIST_H
#define _HOP_LAUNCHER_LIST_H

/*---------------------------------------------------------------------*/
/*    obj_t ...                                                        */
/*    -------------------------------------------------------------    */
/*    The most general type                                            */
/*---------------------------------------------------------------------*/
typedef struct obj {
   char type;
} obj_t;

/*---------------------------------------------------------------------*/
/*    pair_t ...                                                       */
/*---------------------------------------------------------------------*/
typedef struct pair {
   char type;
   void *car;
   struct pair *cdr;
} pair_t;

/*---------------------------------------------------------------------*/
/*    symbol_t ...                                                     */
/*---------------------------------------------------------------------*/
typedef struct symbol {
   char type;
   char chars[ 1 ];
} symbol_t;

/*---------------------------------------------------------------------*/
/*    string_t ...                                                     */
/*---------------------------------------------------------------------*/
typedef struct string {
   char type;
   char chars[ 1 ];
} string_t;

/*---------------------------------------------------------------------*/
/*    integer_t ...                                                    */
/*---------------------------------------------------------------------*/
typedef struct integer {
   char type;
   long val;
} integer_t;

/*---------------------------------------------------------------------*/
/*    Types                                                            */
/*---------------------------------------------------------------------*/
#define TYPE_PAIR    1
#define TYPE_INTEGER 2
#define TYPE_STRING  3
#define TYPE_SYMBOL  4

/*---------------------------------------------------------------------*/
/*    Pairs                                                            */
/*---------------------------------------------------------------------*/
#define PAIRP( o ) ((o) && (((obj_t *)(o))->type == TYPE_PAIR))
#define NULLP( o ) (((pair_t *)(o)) == NIL)

#define NIL 0

#define CAR( o ) (((pair_t *)(o))->car)
#define CDR( o ) (((pair_t *)(o))->cdr)
#define CADR( o ) (CAR( CDR( o ) ) )
#define CDDR( o ) (CDR( CDR( o ) ) )
#define CADDR( o ) (CAR( CDDR( o ) ))
#define CDDDR( o ) (CDR( CDDR( o ) ))

#define SET_CAR( o, v ) (((pair_t *)(o))->car = (v))
#define SET_CDR( o, v ) (((pair_t *)(o))->cdr = (v))

/*---------------------------------------------------------------------*/
/*    Symbols                                                          */
/*---------------------------------------------------------------------*/
#define SYMBOLP( o ) ((o) && (((obj_t *)(o))->type == TYPE_SYMBOL))
#define SYMBOL_CHARS( o ) (&(((symbol_t *)(o))->chars[ 0 ]))
#define SYMBOL_EQ( o1, o2 ) ((o1) == (o2))

/*---------------------------------------------------------------------*/
/*    Strings                                                          */
/*---------------------------------------------------------------------*/
#define STRINGP( o ) ((o) && (((obj_t *)(o))->type == TYPE_STRING))
#define STRING_CHARS( o ) (&(((string_t *)(o))->chars[ 0 ]))
#define STRING_IS( o, s ) (!strcmp( (&(((string_t *)(o))->chars[ 0 ])), s ))

/*---------------------------------------------------------------------*/
/*    Integers                                                         */
/*---------------------------------------------------------------------*/
#define INTEGERP( o ) ((o) && (((obj_t *)(o))->type == TYPE_INTEGER))
#define INTEGER_VAL( o ) (((integer_t *)(o))->val)

/*---------------------------------------------------------------------*/
/*    Export                                                           */
/*---------------------------------------------------------------------*/
extern char *type_name( obj_t * );
extern void display( void *, obj_t * );

extern pair_t *cons( void *a, pair_t *d );
extern pair_t *remq( void *o, pair_t *l );
extern pair_t *memq( void *o, pair_t *l );
extern pair_t *reverse( pair_t *l );
extern pair_t *last_pair( pair_t *l );

extern symbol_t *make_symbol( char * );
extern string_t *make_string( char * );
extern integer_t *make_integer( long );
#endif
