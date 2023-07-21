/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/hopscript/bglhopscript_call.h       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jul 12 07:55:56 2023                          */
/*    Last change :  Thu Jul 20 18:20:58 2023 (serrano)                */
/*    Copyright   :  2023 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Fast C call                                                      */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    JS_CALL1 ...                                                     */
/*---------------------------------------------------------------------*/
#define HOP_CALL0(fun) \
   ((obj_t (*)(obj_t))PROCEDURE_ENTRY(fun))(fun)
#define HOP_CALL1(fun, a0) \
   ((obj_t (*)(obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0)
#define HOP_CALL2(fun, a0, a1) \
   ((obj_t (*)(obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1)
#define HOP_CALL3(fun, a0, a1, a2) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2)
#define HOP_CALL4(fun, a0, a1, a2, a3) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3)
#define HOP_CALL5(fun, a0, a1, a2, a3, a4) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4)
#define HOP_CALL6(fun, a0, a1, a2, a3, a4, a5) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5)
#define HOP_CALL7(fun, a0, a1, a2, a3, a4, a5, a6)		\
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5, a6)
#define HOP_CALL8(fun, a0, a1, a2, a3, a4, a5, a6, a7)		\
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5, a6, a7)
#define HOP_CALL9(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8)	\
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8)
#define HOP_CALL10(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
#define HOP_CALL11(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
#define HOP_CALL12(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
#define HOP_CALL13(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
#define HOP_CALL14(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
#define HOP_CALL15(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
#define HOP_CALL16(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
#define HOP_CALL17(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
#define HOP_CALL18(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) \
   ((obj_t (*)(obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t, obj_t))PROCEDURE_ENTRY(fun))(fun, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

