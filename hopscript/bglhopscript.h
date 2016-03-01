/*===========================================================================*/
/*   (types.scm)                                                             */
/*   Bigloo (4.3a)                                                           */
/*   Inria -- Sophia Antipolis (c)       Sun Feb 28 07:23:57 CET 2016        */
/*===========================================================================*/
/* COMPILATION: (/home/serrano/prgm/project/bigloo/bin/bigloo -O2 -fsharing -L /home/serrano/prgm/project/hop/3.1.x/lib/hop/3.1.0 -srfi bigloo-compile -srfi enable-ssl -srfi enable-threads -srfi enable-avahi -srfi enable-upnp -srfi enable-libuv -cc gcc -srfi hop-dynamic -copt -fPIC -copt -I -copt  types.scm -o bglhopscript.h -hgen)*/

/* Object type definitions */
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

typedef struct BgL_jsindexdescriptorz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_namez00;
   obj_t BgL_configurablez00;
   obj_t BgL_enumerablez00;
   obj_t BgL_writablez00;
   int BgL_indexz00;
} *BgL_jsindexdescriptorz00_bglt;

typedef struct BgL_jsaccessordescriptorz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_namez00;
   obj_t BgL_configurablez00;
   obj_t BgL_enumerablez00;
   obj_t BgL_getz00;
   obj_t BgL_setz00;
} *BgL_jsaccessordescriptorz00_bglt;

typedef struct BgL_jspropertycachez00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_cmapz00;
   long BgL_indexz00;
   obj_t BgL_namez00;
} *BgL_jspropertycachez00_bglt;

typedef struct BgL_jsconstructmapz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_transitionz00;
   obj_t BgL_nextmapz00;
   obj_t BgL_namesz00;
   obj_t BgL_descriptorsz00;
} *BgL_jsconstructmapz00_bglt;

typedef struct BgL_jsstringliteralz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_leftz00;
   obj_t BgL_rightz00;
} *BgL_jsstringliteralz00_bglt;

typedef struct BgL_jsobjectz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
} *BgL_jsobjectz00_bglt;

typedef struct BgL_jswrapperz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_objz00;
   obj_t BgL_dataz00;
} *BgL_jswrapperz00_bglt;

typedef struct BgL_jsglobalobjectz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
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
   struct BgL_jsfunctionz00_bgl * BgL_jszd2float32arrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2float64arrayzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2dataviewzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2booleanzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2stringzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2symbolzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2numberzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2functionzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2functionzd2prototypez00;
   struct BgL_jsmathz00_bgl * BgL_jszd2mathzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2regexpzd2;
   struct BgL_jsregexpz00_bgl * BgL_jszd2regexpzd2prototypez00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2datezd2;
   struct BgL_jsjsonz00_bgl * BgL_jszd2jsonzd2;
   struct BgL_jsservicez00_bgl * BgL_jszd2servicezd2prototypez00;
   obj_t BgL_jszd2hopframezd2prototypez00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2errorzd2;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2syntaxzd2errorz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2typezd2errorz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2urizd2errorz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2evalzd2errorz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2rangezd2errorz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2referencezd2errorz00;
   struct BgL_jsfunctionz00_bgl * BgL_jszd2workerzd2;
   obj_t BgL_jszd2promisezd2;
   struct BgL_jsworkerz00_bgl * BgL_jszd2workerzd2prototypez00;
   struct BgL_jsobjectz00_bgl * BgL_jszd2generatorzd2prototypez00;
   obj_t BgL_jszd2bufferzd2protoz00;
   obj_t BgL_jszd2slowbufferzd2protoz00;
   obj_t BgL_jszd2symbolzd2tablez00;
   obj_t BgL_jszd2symbolzd2iteratorz00;
   obj_t BgL_jszd2symbolzd2speciesz00;
   obj_t BgL_jszd2mainzd2;
} *BgL_jsglobalobjectz00_bglt;

typedef struct BgL_jsarrayz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_inlinez00;
   bool_t BgL_sealedz00;
   bool_t BgL_froza7enza7;
   obj_t BgL_lengthz00;
   obj_t BgL_vecz00;
} *BgL_jsarrayz00_bglt;

typedef struct BgL_jsarraybufferz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   obj_t BgL_dataz00;
} *BgL_jsarraybufferz00_bglt;

typedef struct BgL_jsarraybufferviewz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
} *BgL_jsarraybufferviewz00_bglt;

typedef struct BgL_jstypedarrayz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
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
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
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
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
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
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
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
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
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
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
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
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
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
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
   uint32_t BgL_lengthz00;
   uint32_t BgL_bpez00;
} *BgL_jsuint32arrayz00_bglt;

typedef struct BgL_jsfloat32arrayz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
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
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
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
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_froza7enza7;
   struct BgL_jsarraybufferz00_bgl * BgL_bufferz00;
   obj_t BgL_z52dataz52;
   uint32_t BgL_byteoffsetz00;
} *BgL_jsdataviewz00_bglt;

typedef struct BgL_jsargumentsz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_vecz00;
} *BgL_jsargumentsz00_bglt;

typedef struct BgL_jsstringz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_valz00;
} *BgL_jsstringz00_bglt;

typedef struct BgL_jssymbolz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_valz00;
} *BgL_jssymbolz00_bglt;

typedef struct BgL_jsfunctionz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_namez00;
   obj_t BgL_constructorz00;
   obj_t BgL_allocz00;
   obj_t BgL_constructz00;
   int BgL_constrsiza7eza7;
   obj_t BgL_constrmapz00;
   int BgL_arityz00;
   int BgL_minlenz00;
   int BgL_lenz00;
   bool_t BgL_restz00;
   obj_t BgL_procedurez00;
   obj_t BgL_srcz00;
} *BgL_jsfunctionz00_bglt;

typedef struct BgL_jsservicez00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_namez00;
   obj_t BgL_constructorz00;
   obj_t BgL_allocz00;
   obj_t BgL_constructz00;
   int BgL_constrsiza7eza7;
   obj_t BgL_constrmapz00;
   int BgL_arityz00;
   int BgL_minlenz00;
   int BgL_lenz00;
   bool_t BgL_restz00;
   obj_t BgL_procedurez00;
   obj_t BgL_srcz00;
   obj_t BgL_workerz00;
   obj_t BgL_svcz00;
} *BgL_jsservicez00_bglt;

typedef struct BgL_jshopframez00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
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
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_dataz00;
   struct BgL_serverz00_bgl * BgL_objz00;
} *BgL_jsserverz00_bglt;

typedef struct BgL_jsnumberz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_valz00;
} *BgL_jsnumberz00_bglt;

typedef struct BgL_jsmathz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
} *BgL_jsmathz00_bglt;

typedef struct BgL_jsregexpz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_rxz00;
} *BgL_jsregexpz00_bglt;

typedef struct BgL_jsbooleanz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   bool_t BgL_valz00;
} *BgL_jsbooleanz00_bglt;

typedef struct BgL_jserrorz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_namez00;
   obj_t BgL_msgz00;
   obj_t BgL_stackz00;
   obj_t BgL_fnamez00;
   obj_t BgL_locationz00;
} *BgL_jserrorz00_bglt;

typedef struct BgL_jsdatez00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_valz00;
} *BgL_jsdatez00_bglt;

typedef struct BgL_jsjsonz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
} *BgL_jsjsonz00_bglt;

typedef struct BgL_jsworkerz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_threadz00;
} *BgL_jsworkerz00_bglt;

typedef struct BgL_jspromisez00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_statez00;
   obj_t BgL_valz00;
   obj_t BgL_thensz00;
   obj_t BgL_catchesz00;
   obj_t BgL_workerz00;
   obj_t BgL_resolversz00;
   obj_t BgL_rejectersz00;
   obj_t BgL_watchesz00;
} *BgL_jspromisez00_bglt;

typedef struct BgL_jsgeneratorz00_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL___proto__z00;
   bool_t BgL_extensiblez00;
   obj_t BgL_propertiesz00;
   obj_t BgL_cmapz00;
   obj_t BgL_elementsz00;
   obj_t BgL_z52nextz52;
} *BgL_jsgeneratorz00_bglt;


