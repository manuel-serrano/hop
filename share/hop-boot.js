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
      'etcDir': "/usr/local/etc",
      'binDir': "/usr/local/bin",
      'libDir': "/usr/local/lib",
      'shareDir': "/usr/local/share/hop/3.1.0",
      'contribsDir': "/usr/local/share/hop/contribs",
      'webletsDir': "/usr/local/lib/hop/3.1.0/weblets",
      'serviceBase': "/hop",
      'backend': "native",
      '%modules': {},
      '%requires': {},
      'List': sc_list,
      'Cons': sc_cons
   };
   window.hop[ '%modules' ].hop = window.hop;
}


