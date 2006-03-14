;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __scheme2js_makelib

   (option  (set! *dlopen-init* #t))
   
   (import scheme2js)

   (eval   (export-all)))
