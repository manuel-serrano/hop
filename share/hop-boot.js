/* Automatically generated file (don't edit) */
/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/etc/hop-boot.js.in                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec 15 07:30:58 2015                          */
/*    Last change :  Sat Dec 19 07:30:27 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Client-side bootstrap                                            */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_modules ...                                                  */
/*    -------------------------------------------------------------    */
/*    Builtin modules                                                  */
/*---------------------------------------------------------------------*/
if( !('hop' in window) ) {
   window.hop = {
      'version': "3.1.0",
      'build': "@BUILD@",
      'etcDir': "/tmp/HOP-3.1.x/etc",
      'binDir': "/tmp/HOP-3.1.x/bin",
      'libDir': "/tmp/HOP-3.1.x/lib",
      'shareDir': "/tmp/HOP-3.1.x/share/hop/3.1.0",
      'contribsDir': "/tmp/HOP-3.1.x/share/hop/contribs",
      'webletsDir': "/tmp/HOP-3.1.x/lib/hop/3.1.0/weblets",
      'serviceBase': "/hop",
      'backend': "native",
      '%modules': {},
      '%requires': {},
      'List': sc_list,
      'Cons': sc_cons
   };
   window.hop[ '%modules' ].hop = window.hop;
}


