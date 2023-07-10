;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/usage-bit.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 14 17:16:48 2019                          */
;*    Last change :  Mon Jul 10 08:47:13 2023 (serrano)                */
;*    Copyright   :  2019-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Usage bit names                                                  */
;*    -------------------------------------------------------------    */
;*    The usage bits are:                                              */
;*      init: variable initialized (at some point)                     */
;*      new: variable used as a constructor                            */
;*      ref: variable referenced                                       */
;*      assig: variable assigned                                       */
;*      get: variable used to get an object property                   */
;*      set: variable used to set an object property                   */
;*      call: variable called as a function                            */
;*      delete: variable used in a delete operation                    */
;*      instanceof: variable used as an instanceof rhs                 */
;*      uninit: variable potentially used before initialization        */
;*      rest: is a rest argument                                       */
;*      method: a method of the object is called (e.g. o.f())          */
;*      &ref: a variable whose stack address is used                   */
;*                                                                     */
;*    The following bit are used for optimizing "arguments" values     */
;*    (see arguments.scm):                                             */
;*      slice: variable used in a slice call (apply or call)           */
;*      length: variable used in a VAR.length expression               */
;*      aref: variable used in a integer array indexed access          */
;*      apply: variable used in the second position of an apply call   */
;*      spread: used in spread call                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    usage-keys->bit ...                                              */
;*---------------------------------------------------------------------*/
(define (usage-keys->bit keys)
   (let ((u #u32:0))
      (for-each (lambda (k) (set! u (bit-oru32 u (usage-key->bit k)))) keys)
      u))

;*---------------------------------------------------------------------*/
;*    usage-key->bit ...                                               */
;*---------------------------------------------------------------------*/
(define (usage-key->bit key)
   (case key
      ((assig) #u32:1)
      ((init) #u32:2)
      ((new) #u32:4)
      ((ref) #u32:8)
      ((eval) #u32:16)
      ((get) #u32:32)
      ((set) #u32:64)
      ((call) #u32:128)
      ((delete) #u32:256)
      ((instanceof) #u32:512)
      ((uninit) #u32:1024)
      ((rest) #u32:2048)
      ((method) #u32:4096)
      ((&ref) #u32:8192)
      ((slice) #u32:16384)
      ((length) #u32:32768)
      ((aref) #u32:65536)
      ((apply) #u32:131072)
      ((spread) #u32:262144)
      ((iref) #u32:524288)
      (else (error "usage-key->bit" "Illegal key" key))))

;*---------------------------------------------------------------------*/
;*    usage-bit->key ...                                               */
;*---------------------------------------------------------------------*/
(define (usage-bit->key bit)
   (case bit
      ((#u32:1) 'assig)
      ((#u32:2) 'init)
      ((#u32:4) 'new)
      ((#u32:8) 'ref)
      ((#u32:16) 'eval)
      ((#u32:32) 'get)
      ((#u32:64) 'set)
      ((#u32:128) 'call)
      ((#u32:256) 'delete)
      ((#u32:512) 'instanceof)
      ((#u32:1024) 'uninit)
      ((#u32:2048) 'rest)
      ((#u32:4096) 'method)
      ((#u32:8192) '&ref)
      ((#u32:16384) 'slice)
      ((#u32:32768) 'length)
      ((#u32:65536) 'aref)
      ((#u32:131072) 'apply)
      ((#u32:262144) 'spread)
      ((#u32:524288) 'iref)
      (else (error "usage-bit->key" "Illegal key" bit))))
   
