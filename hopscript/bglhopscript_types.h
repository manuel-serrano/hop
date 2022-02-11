/*===========================================================================*/
/*   (types.scm)                                                             */
/*   Bigloo (4.5a)                                                           */
/*   Inria -- Sophia Antipolis (c)       Fri Feb 4 08:42:28 AM CET 2022      */
/*===========================================================================*/
/* COMPILATION: (bigloo -O3 -fstackable -fsharing -L /home/serrano/prgm/project/hop/hop/lib/hop/3.6.0 -srfi bigloo-compile -srfi enable-tls -srfi license-academic -srfi enable-ssl -srfi enable-threads -srfi enable-avahi -srfi enable-upnp -srfi enable-libuv -srfi hop-dynamic -copt -fPIC -copt -DBGL_NAN_TAGGING=0 -copt -I -copt  types.scm -o bglhopscript_types.h -hgen) */

/* object type definitions */
typedef struct BgL_workerhopthreadz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_namez00;
   obj_t BgL_bodyz00;
   bool_t BgL_detachedpz00;
   obj_t BgL_endzd2resultzd2;
   obj_t BgL_endzd2exceptionzd2;
   void * BgL_z42builtinz42;
   obj_t BgL_z52loadingzd2filez80;
   obj_t BgL_z52loopz52;
   bool_t BgL_keepzd2alivezd2;
   obj_t BgL_mutexz00;
   obj_t BgL_condvz00;
   obj_t BgL_prehookz00;
   obj_t BgL_alivepz00;
   obj_t BgL_tqueuez00;
   obj_t BgL_listenersz00;
   obj_t BgL_exitlistenersz00;
   obj_t BgL_errorlistenersz00;
   obj_t BgL_onmessagez00;
   obj_t BgL_onexitz00;
   struct BgL_jsglobalobjectz00_bgl * BgL_z52thisz52;
   obj_t BgL_z52processz52;
   int BgL_z52retvalz52;
   obj_t BgL_asyncz00;
   obj_t BgL_statez00;
   obj_t BgL_modulezd2cachezd2;
   obj_t BgL_parentz00;
   obj_t BgL_subworkersz00;
   obj_t BgL_uvhandlesz00;
   obj_t BgL_callz00;
   obj_t BgL_handlersz00;
   obj_t BgL_servicesz00;
   obj_t BgL_z52exnz52;
} *BgL_workerhopthreadz00_bglt;

typedef struct BgL_messageeventz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_namez00;
   obj_t BgL_targetz00;
   bool_t BgL_stoppedz00;
   obj_t BgL_valuez00;
   obj_t BgL_dataz00;
} *BgL_messageeventz00_bglt;

typedef struct BgL_jspropertydescriptorz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_namez00;
   obj_t BgL_configurablez00;
   obj_t BgL_enumerablez00;
} *BgL_jspropertydescriptorz00_bglt;

typedef struct BgL_jsdatadescriptorz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_namez00;
   obj_t BgL_configurablez00;
   obj_t BgL_enumerablez00;
   obj_t BgL_writablez00;
} *BgL_jsdatadescriptorz00_bglt;

typedef struct BgL_jsvaluedescriptorz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_namez00;
   obj_t BgL_configurablez00;
   obj_t BgL_enumerablez00;
   obj_t BgL_writablez00;
   obj_t BgL_valuez00;
} *BgL_jsvaluedescriptorz00_bglt;

typedef struct BgL_jsaccessordescriptorz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_namez00;
   obj_t BgL_configurablez00;
   obj_t BgL_enumerablez00;
   obj_t BgL_getz00;
   obj_t BgL_z52getz52;
   obj_t BgL_setz00;
   obj_t BgL_z52setz52;
} *BgL_jsaccessordescriptorz00_bglt;

typedef struct BgL_jswrapperdescriptorz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_namez00;
   obj_t BgL_configurablez00;
   obj_t BgL_enumerablez00;
   obj_t BgL_writablez00;
   obj_t BgL_z52getz52;
   obj_t BgL_z52setz52;
} *BgL_jswrapperdescriptorz00_bglt;

typedef struct BgL_jspropertycachez00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_imapz00;
   struct BgL_jsconstructmapz00_bgl * BgL_emapz00;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   struct BgL_jsconstructmapz00_bgl * BgL_pmapz00;
   struct BgL_jsconstructmapz00_bgl * BgL_nmapz00;
   struct BgL_jsconstructmapz00_bgl * BgL_amapz00;
   struct BgL_jsconstructmapz00_bgl * BgL_xmapz00;
   obj_t BgL_nextemapz00;
   obj_t BgL_nextnmapz00;
   long BgL_iindexz00;
   long BgL_eindexz00;
   long BgL_cindexz00;
   long BgL_pindexz00;
   long BgL_nindexz00;
   long BgL_aindexz00;
   long BgL_vindexz00;
   obj_t BgL_ownerz00;
   obj_t BgL_srcz00;
   long BgL_pointz00;
   struct BgL_jsstringliteralz00_bgl * BgL_namez00;
   obj_t BgL_methodz00;
   obj_t BgL_functionz00;
   obj_t BgL_pctablez00;
   obj_t BgL_usagez00;
   bool_t BgL_registeredz00;
   uint32_t BgL_cntmissz00;
   uint32_t BgL_cntimapz00;
   uint32_t BgL_cntemapz00;
   uint32_t BgL_cntcmapz00;
   uint32_t BgL_cntpmapz00;
   uint32_t BgL_cntnmapz00;
   uint32_t BgL_cntamapz00;
   uint32_t BgL_cntxmapz00;
   uint32_t BgL_cntvtablez00;
} *BgL_jspropertycachez00_bglt;

typedef struct BgL_jsconstructmapz00_bgl {
   header_t header;
   obj_t widening;
   uint32_t BgL_z52idz52;
   obj_t BgL_lockz00;
   obj_t BgL_propsz00;
   obj_t BgL_methodsz00;
   obj_t BgL_transitionsz00;
   long BgL_detachcntz00;
   obj_t BgL_detachlocsz00;
   obj_t BgL_ctorz00;
   bool_t BgL_singlez00;
   obj_t BgL_vtablez00;
   struct BgL_jsconstructmapz00_bgl * BgL_parentz00;
   obj_t BgL_mptablez00;
   obj_t BgL_mrtablez00;
   obj_t BgL_mntablez00;
} *BgL_jsconstructmapz00_bglt;

typedef struct BgL_jsstringliteralz00_bgl {
   header_t header;
   obj_t widening;
   uint32_t BgL_lengthz00;
   obj_t BgL_leftz00;
   obj_t BgL_rightz00;
} *BgL_jsstringliteralz00_bglt;

typedef struct BgL_jsstringliteralasciiz00_bgl {
   header_t header;
   obj_t widening;
   uint32_t BgL_lengthz00;
   obj_t BgL_leftz00;
   obj_t BgL_rightz00;
} *BgL_jsstringliteralasciiz00_bglt;

typedef struct BgL_jsstringliteralindexz00_bgl {
   header_t header;
   obj_t widening;
   uint32_t BgL_lengthz00;
   obj_t BgL_leftz00;
   obj_t BgL_rightz00;
   uint32_t BgL_indexz00;
} *BgL_jsstringliteralindexz00_bglt;

typedef struct BgL_jsstringliteralsubstringz00_bgl {
   header_t header;
   obj_t widening;
   uint32_t BgL_lengthz00;
   obj_t BgL_leftz00;
   obj_t BgL_rightz00;
} *BgL_jsstringliteralsubstringz00_bglt;

typedef struct BgL_jsstringliteralbufferz00_bgl {
   header_t header;
   obj_t widening;
   uint32_t BgL_lengthz00;
   obj_t BgL_leftz00;
   obj_t BgL_rightz00;
} *BgL_jsstringliteralbufferz00_bglt;

typedef struct BgL_jsstringliteralutf8z00_bgl {
   header_t header;
   obj_t widening;
   uint32_t BgL_lengthz00;
   obj_t BgL_leftz00;
   obj_t BgL_rightz00;
   long BgL_z52idxutf8z52;
   long BgL_z52idxstrz52;
   uint32_t BgL_z52culenz52;
} *BgL_jsstringliteralutf8z00_bglt;

typedef struct BgL_jsobjectz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
} *BgL_jsobjectz00_bglt;

typedef struct BgL_jsrecordz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
} *BgL_jsrecordz00_bglt;

typedef struct BgL_jswrapperz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_objz00;
   obj_t BgL_dataz00;
} *BgL_jswrapperz00_bglt;

typedef struct BgL_jsarrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_ilenz00;
   obj_t BgL_vecz00;
} *BgL_jsarrayz00_bglt;

typedef struct BgL_jsarraybufferz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   obj_t BgL_dataz00;
} *BgL_jsarraybufferz00_bglt;

typedef struct BgL_jsarraybufferviewz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
} *BgL_jsarraybufferviewz00_bglt;

typedef struct BgL_jstypedarrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jstypedarrayz00_bglt;

typedef struct BgL_jsint8arrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jsint8arrayz00_bglt;

typedef struct BgL_jsuint8arrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jsuint8arrayz00_bglt;

typedef struct BgL_jsuint8clampedarrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jsuint8clampedarrayz00_bglt;

typedef struct BgL_jsint16arrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jsint16arrayz00_bglt;

typedef struct BgL_jsuint16arrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jsuint16arrayz00_bglt;

typedef struct BgL_jsint32arrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jsint32arrayz00_bglt;

typedef struct BgL_jsuint32arrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jsuint32arrayz00_bglt;

typedef struct BgL_jsbigint64arrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jsbigint64arrayz00_bglt;

typedef struct BgL_jsbiguint64arrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jsbiguint64arrayz00_bglt;

typedef struct BgL_jsfloat32arrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jsfloat32arrayz00_bglt;

typedef struct BgL_jsfloat64arrayz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jsfloat64arrayz00_bglt;

typedef struct BgL_jsdataviewz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
} *BgL_jsdataviewz00_bglt;

typedef struct BgL_jsargumentsz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_vecz00;
} *BgL_jsargumentsz00_bglt;

typedef struct BgL_jsstringz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_valz00;
} *BgL_jsstringz00_bglt;

typedef struct BgL_jssymbolz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_valz00;
} *BgL_jssymbolz00_bglt;

typedef struct BgL_jssymbolliteralz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsstringliteralz00_bgl * BgL_valz00;
} *BgL_jssymbolliteralz00_bglt;

typedef struct BgL_jsprocedurez00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_procedurez00;
   int BgL_arityz00;
} *BgL_jsprocedurez00_bglt;

typedef struct BgL_jsprocedureinfoz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_procedurez00;
   int BgL_arityz00;
   obj_t BgL_infoz00;
} *BgL_jsprocedureinfoz00_bglt;

typedef struct BgL_jsfunctionz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_procedurez00;
   int BgL_arityz00;
   obj_t BgL_infoz00;
   int BgL_constrsiza7eza7;
   obj_t BgL_allocz00;
   struct BgL_jsconstructmapz00_bgl * BgL_constrmapz00;
   obj_t BgL_prototypez00;
} *BgL_jsfunctionz00_bglt;

typedef struct BgL_jsmethodz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_procedurez00;
   int BgL_arityz00;
   obj_t BgL_infoz00;
   int BgL_constrsiza7eza7;
   obj_t BgL_allocz00;
   struct BgL_jsconstructmapz00_bgl * BgL_constrmapz00;
   obj_t BgL_prototypez00;
   obj_t BgL_methodz00;
} *BgL_jsmethodz00_bglt;

typedef struct BgL_jsservicez00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_procedurez00;
   int BgL_arityz00;
   obj_t BgL_infoz00;
   int BgL_constrsiza7eza7;
   obj_t BgL_allocz00;
   struct BgL_jsconstructmapz00_bgl * BgL_constrmapz00;
   obj_t BgL_prototypez00;
   obj_t BgL_workerz00;
   obj_t BgL_svcz00;
} *BgL_jsservicez00_bglt;

typedef struct BgL_jsclassz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_procedurez00;
   int BgL_arityz00;
   obj_t BgL_infoz00;
   int BgL_constrsiza7eza7;
   obj_t BgL_allocz00;
   struct BgL_jsconstructmapz00_bgl * BgL_constrmapz00;
   obj_t BgL_prototypez00;
   obj_t BgL_constructorz00;
   obj_t BgL_claza7za7z00;
} *BgL_jsclassz00_bglt;

typedef struct BgL_jshopframez00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_z52thisz52;
   obj_t BgL_argsz00;
   obj_t BgL_srvz00;
   obj_t BgL_optionsz00;
   obj_t BgL_headerz00;
   obj_t BgL_pathz00;
} *BgL_jshopframez00_bglt;

typedef struct BgL_jsserverz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_dataz00;
   struct BgL_serverz00_bgl * BgL_objz00;
} *BgL_jsserverz00_bglt;

typedef struct BgL_jsnumberz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_valz00;
} *BgL_jsnumberz00_bglt;

typedef struct BgL_jsbigintz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_valz00;
} *BgL_jsbigintz00_bglt;

typedef struct BgL_jsmathz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
} *BgL_jsmathz00_bglt;

typedef struct BgL_jsregexpz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_rxz00;
   obj_t BgL_sourcez00;
   uint32_t BgL_flagsz00;
} *BgL_jsregexpz00_bglt;

typedef struct BgL_jsbooleanz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_valz00;
} *BgL_jsbooleanz00_bglt;

typedef struct BgL_jserrorz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_namez00;
   obj_t BgL_msgz00;
   obj_t BgL_z52thisz52;
   obj_t BgL_stackz00;
   obj_t BgL_fnamez00;
   obj_t BgL_locationz00;
} *BgL_jserrorz00_bglt;

typedef struct BgL_jsdatez00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_valz00;
} *BgL_jsdatez00_bglt;

typedef struct BgL_jsjsonz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
} *BgL_jsjsonz00_bglt;

typedef struct BgL_jsmodulez00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_z52modulez52;
   obj_t BgL_evarsz00;
   obj_t BgL_defaultexportz00;
   obj_t BgL_exportsz00;
   obj_t BgL_namespacez00;
   obj_t BgL_importsz00;
   obj_t BgL_redirectsz00;
   obj_t BgL_defaultz00;
   obj_t BgL_checksumz00;
} *BgL_jsmodulez00_bglt;

typedef struct BgL_jsworkerz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_threadz00;
} *BgL_jsworkerz00_bglt;

typedef struct BgL_jspromisez00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_statez00;
   obj_t BgL_valz00;
   obj_t BgL_thensz00;
   obj_t BgL_catchesz00;
   obj_t BgL_resolverz00;
   obj_t BgL_rejecterz00;
   obj_t BgL_workerz00;
   obj_t BgL_z52thisz52;
   obj_t BgL_z52namez52;
} *BgL_jspromisez00_bglt;

typedef struct BgL_jsgeneratorz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_z52nextz52;
   obj_t BgL_z52envz52;
} *BgL_jsgeneratorz00_bglt;

typedef struct BgL_jsyieldz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_z52vheaderz52;
   unsigned long BgL_z52vlengthz52;
   obj_t BgL_z52vobj0z52;
   obj_t BgL_z52vobj1z52;
} *BgL_jsyieldz00_bglt;

typedef struct BgL_jsproxyz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   struct BgL_jsobjectz00_bgl * BgL_handlerz00;
   obj_t BgL_getcachez00;
   obj_t BgL_setcachez00;
   obj_t BgL_applycachez00;
} *BgL_jsproxyz00_bglt;

typedef struct BgL_jsmapz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_mapdataz00;
   obj_t BgL_vecz00;
   long BgL_cursorz00;
} *BgL_jsmapz00_bglt;

typedef struct BgL_jsglobalobjectz00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsconstructmapz00_bgl * BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_namez00;
   obj_t BgL_workerz00;
   struct BgL_jsobjectz00_bgl * BgL_jszd2hopzd2builtinz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2objectzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2arrayzd2;
   struct BgL_jsarrayz00_bgl * BgL_jszd2arrayzd2prototypez00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2arraybufferzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2int8arrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2uint8arrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2uint8clampedarrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2int16arrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2uint16arrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2int32arrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2uint32arrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2bigint64arrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2biguint64arrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2float32arrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2float64arrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2dataviewzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2vectorzd2;
   struct BgL_jsarrayz00_bgl * BgL_jszd2vectorzd2prototypez00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2booleanzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2stringzd2;
   struct BgL_jsstringz00_bgl * BgL_jszd2stringzd2prototypez00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2symbolzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2numberzd2;
   struct BgL_jsnumberz00_bgl * BgL_jszd2numberzd2prototypez00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2bigintzd2;
   struct BgL_jsbigintz00_bgl * BgL_jszd2bigintzd2prototypez00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2functionzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2functionzd2prototypez00;
   struct BgL_jsobjectz00_bgl * BgL_jszd2functionzd2strictzd2prototypezd2;
   struct BgL_jsobjectz00_bgl * BgL_jszd2mathzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2regexpzd2;
   struct BgL_jsregexpz00_bgl * BgL_jszd2regexpzd2prototypez00;
   obj_t BgL_jszd2regexpzd2lastzd2matchzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2datezd2;
   struct BgL_jsdatez00_bgl * BgL_jszd2datezd2prototypez00;
   struct BgL_jsobjectz00_bgl * BgL_jszd2jsonzd2;
   struct BgL_jsservicez00_bgl * BgL_jszd2servicezd2prototypez00;
   obj_t BgL_jszd2hopframezd2prototypez00;
   obj_t BgL_jszd2serverzd2prototypez00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2errorzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2syntaxzd2errorz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2typezd2errorz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2urizd2errorz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2evalzd2errorz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2rangezd2errorz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2referencezd2errorz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2workerzd2;
   obj_t BgL_jszd2promisezd2;
   obj_t BgL_jszd2proxyzd2;
   struct BgL_jsworkerz00_bgl * BgL_jszd2workerzd2prototypez00;
   struct BgL_jsobjectz00_bgl * BgL_jszd2generatorzd2prototypez00;
   struct BgL_jsobjectz00_bgl * BgL_jszd2generatorfunctionzd2prototypez00;
   obj_t BgL_jszd2bufferzd2protoz00;
   obj_t BgL_jszd2slowbufferzd2protoz00;
   obj_t BgL_jszd2symbolzd2ctorz00;
   obj_t BgL_jszd2symbolzd2tablez00;
   obj_t BgL_jszd2symbolzd2iteratorz00;
   obj_t BgL_jszd2symbolzd2speciesz00;
   obj_t BgL_jszd2symbolzd2hasinstancez00;
   obj_t BgL_jszd2symbolzd2tostringtagz00;
   obj_t BgL_jszd2symbolzd2unscopablesz00;
   obj_t BgL_jszd2mainzd2;
   obj_t BgL_jszd2callzd2;
   obj_t BgL_jszd2applyzd2;
   obj_t BgL_jszd2vindexzd2;
   obj_t BgL_jszd2inputzd2portz00;
   obj_t BgL_jszd2newzd2targetz00;
   obj_t BgL_jszd2functionzd2prototypezd2propertyzd2rwz00;
   obj_t BgL_jszd2functionzd2prototypezd2propertyzd2roz00;
   obj_t BgL_jszd2functionzd2prototypezd2propertyzd2nullz00;
   obj_t BgL_jszd2functionzd2prototypezd2propertyzd2undefinedz00;
   obj_t BgL_jszd2functionzd2strictzd2elementszd2;
   obj_t BgL_jszd2xmlzd2markupsz00;
   obj_t BgL_charzd2tablezd2;
   obj_t BgL_jszd2regexpzd2positionsz00;
   obj_t BgL_jszd2nodejszd2pcachez00;
   obj_t BgL_jszd2objectzd2pcachez00;
   obj_t BgL_jszd2arrayzd2pcachez00;
   obj_t BgL_jszd2functionzd2pcachez00;
   obj_t BgL_jszd2regexpzd2pcachez00;
   obj_t BgL_jszd2stringzd2pcachez00;
   obj_t BgL_jszd2stringliteralzd2pcachez00;
   obj_t BgL_jszd2spawnzd2pcachez00;
   obj_t BgL_jszd2servicezd2pcachez00;
   obj_t BgL_jszd2numberzd2pcachez00;
   obj_t BgL_jszd2jsonzd2pcachez00;
   obj_t BgL_jszd2proxyzd2pcachez00;
   obj_t BgL_jszd2generatorzd2pcachez00;
   obj_t BgL_jszd2initialzd2cmapz00;
   obj_t BgL_jszd2argumentszd2cmapz00;
   obj_t BgL_jszd2strictzd2argumentszd2cmapzd2;
   obj_t BgL_jszd2arrayzd2cmapz00;
   obj_t BgL_jszd2functionzd2cmapz00;
   obj_t BgL_jszd2functionzd2sanszd2prototypezd2cmapz00;
   obj_t BgL_jszd2functionzd2strictzd2cmapzd2;
   obj_t BgL_jszd2functionzd2strictzd2bindzd2cmapz00;
   obj_t BgL_jszd2functionzd2writablezd2cmapzd2;
   obj_t BgL_jszd2functionzd2writablezd2strictzd2cmapz00;
   obj_t BgL_jszd2functionzd2prototypezd2cmapzd2;
   obj_t BgL_jszd2generatorzd2cmapz00;
   obj_t BgL_jszd2yieldzd2cmapz00;
   obj_t BgL_jszd2regexpzd2cmapz00;
   obj_t BgL_jszd2regexpzd2execzd2cmapzd2;
   obj_t BgL_jszd2scopezd2cmapz00;
   obj_t BgL_jszd2datezd2cmapz00;
   obj_t BgL_jszd2propertyzd2descriptorzd2valuezd2cmapz00;
   obj_t BgL_jszd2propertyzd2descriptorzd2getterzd2cmapz00;
} *BgL_jsglobalobjectz00_bglt;

typedef struct BgL_jsresponsez00_bgl {
   header_t header;
   obj_t widening;
   struct BgL_jsglobalobjectz00_bgl * BgL_z52thisz52;
   struct BgL_jsobjectz00_bgl * BgL_objz00;
} *BgL_jsresponsez00_bglt;


