/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/etc/node_crypto_groups.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jun  3 14:41:10 2015                          */
/*    Last change :  Wed Jun  3 16:42:17 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    This tools is used to automatize the interface between           */
/*    Nodejs node_crypto_groups.h declaration file and the             */
/*    Hop _crypto.scm module.                                          */
/*=====================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include "node_crypto_groups.h"

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    main ...                                                         */
/*---------------------------------------------------------------------*/
int
main( int argc, char *argvp[] ) {
   int i;

   puts( "(define modp_groups '(" );
   for( i = 0; ; i++ ) {
      unsigned char *c;
      int j;
      if( modp_groups[ i ].name != 0 ) {
	 fprintf( stdout, "  (\"%s\" (\"", modp_groups[ i ].name );
	 for( j = 0; j < modp_groups[ i ].prime_size ; j++ ) {
	    fprintf( stdout, "\\x%02x", modp_groups[ i ].prime[ j ] );
	 }
	 fprintf( stdout, "\" . \"\\x02\"))\n" );
      } else {
	 break;
      }
   }
   puts( "))\n" );
   return 0;
}
      
   
