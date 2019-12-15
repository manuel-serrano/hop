;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/usage-bit.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 14 17:16:48 2019                          */
;*    Last change :  Sun Dec 15 07:16:31 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
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
      ((assig) #u32:16)
      ((get) #u32:32)
      ((set) #u32:64)
      ((call) #u32:128)
      ((delete) #u32:256)
      ((instanceof) #u32:512)
      ((uninit) #u32:1024)
      ((rest) #u32:2048)
      ((eval) #u32:4096)
      (else (error "usage-key->bit" "Illegal key" key))))

;*---------------------------------------------------------------------*/
;*    usage-bit->key ...                                               */
;*---------------------------------------------------------------------*/
(define (usage-bit->key bit)
   (case bit
      ((#u32:1) 'assig)
      ((#u32:1) 'assig)
      ((#u32:2) 'init)
      ((#u32:4) 'new)
      ((#u32:8) 'ref)
      ((#u32:16) 'assig)
      ((#u32:32) 'get)
      ((#u32:64) 'set)
      ((#u32:128) 'call)
      ((#u32:256) 'delete)
      ((#u32:512) 'instanceof)
      ((#u32:1024) 'uninit)
      ((#u32:2048) 'rest)
      ((#u32:4096) 'eval)
      (else (error "usage-bit->key" "Illegal key" bit))))
   
