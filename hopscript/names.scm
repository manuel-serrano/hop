;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/names.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar 30 06:29:09 2019                          */
;*    Last change :  Tue Apr  2 11:28:25 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Property names (see stringliteral.scm)                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_names

   (include "types.sch")

   (library hop)

   (import __hopscript_types)
   
   (use    __hopscript_stringliteral
	   __hopscript_lib
	   __hopscript_public)

   (export (inline js-name-pcache::obj ::JsStringLiteral)
	   (inline js-name-pcache-set! ::JsStringLiteral ::JsPropertyCache)
	   (inline js-jsstring-toname::JsStringLiteral ::JsStringLiteral)
	   (js-toname::obj ::obj ::JsGlobalObject)
	   (inline js-jsstring-name ::JsStringLiteral)
	   (inline js-name->string::bstring ::JsStringLiteral)
	   (js-jsstring->name!::JsStringLiteral ::JsStringLiteral)
	   (js-ascii-name->jsstring::JsStringLiteralASCII ::bstring)
	   (js-utf8-name->jsstring::JsStringLiteralUTF8 ::bstring)
	   (js-integer-name->jsstring::JsStringLiteralASCII ::long)
	   (js-name->jsstring::JsStringLiteral ::bstring)
	   ;; known names
	   &empty
	   &__proto__
	   &Array
	   &Buffer
	   &GLOBAL
	   &Object
	   &String
	   &Worker
	   &charAt
	   &charCodeAt
	   &compiler
	   &configurable
	   &console
	   &constructor
	   &done
	   &enumerable
	   &exec
	   &exports
	   &for
	   &forEach
	   &global
	   &hop
	   &indexOf
	   &iterator
	   &keyFor
	   &lastIndex
	   &lastIndexOf
	   &length
	   &localeCompare
	   &log
	   &match
	   &map
	   &module
	   &naturalCompare
	   &next
	   &prototype
	   &replace
	   &require
	   &return
	   &slice
	   &split
	   &substr
	   &substring
	   &toLowerCase
	   &toLocaleLowerCase
	   &toUpperCase
	   &toLocaleUpperCase
	   &toString
	   &trim
	   &value
	   &valueOf
	   &writable))

;*---------------------------------------------------------------------*/
;*    js-names-mutex ...                                               */
;*---------------------------------------------------------------------*/
(define js-names-mutex (make-mutex "js-names"))

;*---------------------------------------------------------------------*/
;*    names ...                                                        */
;*---------------------------------------------------------------------*/
(define names
   (create-hashtable :eqtest string=? :hash string-hash-number))

;*---------------------------------------------------------------------*/
;*    js-name-pcache ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (js-name-pcache::obj o::JsStringLiteral)
   (with-access::JsStringLiteral o (pcache)
      pcache))

;*---------------------------------------------------------------------*/
;*    js-name-pcache-set! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (js-name-pcache-set! o::JsStringLiteral c::JsPropertyCache)
   (with-access::JsStringLiteral o (pcache)
      (set! pcache c)))

;*---------------------------------------------------------------------*/
;*    js-jsstring-name ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-name o::JsStringLiteral)
   (object-widening o))

;*---------------------------------------------------------------------*/
;*    js-jsstring-name-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-name-set! o::JsStringLiteral name)
   (object-widening-set! o name))

;*---------------------------------------------------------------------*/
;*    js-jsstring-toname ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (js-jsstring-toname::JsStringLiteral p::JsStringLiteral)
   (or (js-jsstring-name p) (js-jsstring->name! p)))

;*---------------------------------------------------------------------*/
;*    js-toname ...                                                    */
;*    -------------------------------------------------------------    */
;*    www.ecma-international.org/ecma-262/7.0/#sec-topropertykey       */
;*---------------------------------------------------------------------*/
(define (js-toname p %this)
   (let loop ((p p))
      (cond
	 ((isa? p JsStringLiteral)
	  (js-jsstring-toname p))
	 ((fixnum? p)
	  (js-integer-name->jsstring p))
	 ((uint32? p)
	  (cond-expand
	     (bint30
	      (if (<u32 p (fixnum->uint32 (bit-lsh 1 29)))
		  (js-integer-name->jsstring (uint32->fixnum p))
		  (js-ascii-name->jsstring (llong->string (uint32->llong p)))))
	     (bint32
	      (if (<u32 p (bit-lshu32 (fixnum->uint32 1) 31))
		  (js-integer-name->jsstring (uint32->fixnum p))
		  (js-ascii-name->jsstring (llong->string (uint32->llong p)))))
	     (else
	      (js-integer-name->jsstring (uint32->fixnum p)))))
	 ((int32? p)
	  (cond-expand
	     (bint30
	      (if (and (>s32 p 0) (<s32 p (fixnum->int32 (bit-lsh 1 29))))
		  (js-integer-name->jsstring (int32->fixnum p))
		  (js-ascii-name->jsstring (llong->string (int32->llong p)))))
	     (bint32
	      (js-ascii-name->jsstring (fixnum->string (int32->fixnum p))))
	     (else
	      (js-integer-name->jsstring (int32->fixnum p)))))
	 ((isa? p JsSymbolLiteral)
	  p)
	 ((isa? p JsSymbol)
	  (with-access::JsSymbol p (val)
	     val))
	 ((symbol? p)
	  (error "js-toname" "Illegal `symbol'" p))
	 ((string? p)
	  (error "js-toname" "Illegal `string'" p))
	 (else
	  (loop (js-tostring p %this))))))

;*---------------------------------------------------------------------*/
;*    js-name->string ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (js-name->string::bstring o::JsStringLiteral)
   (with-access::JsStringLiteral o (left)
      left))

;*---------------------------------------------------------------------*/
;*    js-jsstring->name! ...                                           */
;*---------------------------------------------------------------------*/
(define (js-jsstring->name!::JsStringLiteral o::JsStringLiteral)
   ;; call js-jsstring->string as the string must be normalized
   ;; before being potentially added in the name hashtable
   (let ((str (js-jsstring->string o)))
      (with-lock js-names-mutex
	 (let ((n (hashtable-get names str)))
	    (unless n
	       (set! n o)
	       (hashtable-put! names str n))
	    (js-jsstring-name-set! o n)
	    n))))

;*---------------------------------------------------------------------*/
;*    js-ascii-name->jsstring ...                                      */
;*---------------------------------------------------------------------*/
(define (js-ascii-name->jsstring::JsStringLiteralASCII str::bstring)
   (with-lock js-names-mutex
      (lambda ()
	 (let ((n (hashtable-get names str)))
	    (or n
		(let ((n (instantiate::JsStringLiteralASCII
			    (weight (fixnum->uint32 (string-length str)))
			    (left str)
			    (right #f))))
		   (hashtable-put! names str n)
		   (js-jsstring-name-set! n n)
		   n))))))

;*---------------------------------------------------------------------*/
;*    js-index-name->jsstring ...                                      */
;*---------------------------------------------------------------------*/
(define (js-index-name->jsstring::JsStringLiteralASCII num::uint32)
   (with-lock js-names-mutex
      (lambda ()
	 (let ((n (hashtable-get names num)))
	    (or n
		(let* ((str (fixnum->string (uint32->fixnum num)))
		       (n (instantiate::JsStringLiteralIndex
			     (weight (string-length str))
			     (left str)
			     (right #f)
			     (index num))))
		   (hashtable-put! names str n)
		   (js-jsstring-name-set! n n)
		   n))))))

;*---------------------------------------------------------------------*/
;*    js-utf8-name->jsstring ...                                       */
;*---------------------------------------------------------------------*/
(define (js-utf8-name->jsstring str::bstring)
   (with-lock js-names-mutex
      (let ((n (hashtable-get names str)))
	 (or n
	     (let ((n (instantiate::JsStringLiteralUTF8
			 (weight (fixnum->uint32 (string-length str)))
			 (left str)
			 (right #f))))
		(hashtable-put! names str n)
		(js-jsstring-name-set! n n)
		n)))))

;*---------------------------------------------------------------------*/
;*    js-name->jsstring ...                                            */
;*---------------------------------------------------------------------*/
(define (js-name->jsstring::JsStringLiteral str::bstring)
   (let ((enc (string-minimal-charset str)))
      (case enc
	 ((ascii) (js-ascii-name->jsstring str))
	 ((latin1 utf8) (js-utf8-name->jsstring str))
	 (else (error "js-name->jsstring" "unsupported encoding" enc)))))

;*---------------------------------------------------------------------*/
;*    js-integer-name->jsstring ...                                    */
;*---------------------------------------------------------------------*/
(define (js-integer-name->jsstring num::long)
   (cond
      ((and (>fx num -10) (<fx num 100))
       (vector-ref &integers (+fx num 10)))
      ((and (>fx num 0) (<fx num 65535))
       (js-index-name->jsstring (fixnum->uint32 num)))
      (else
       (js-ascii-name->jsstring (fixnum->string num)))))

;*---------------------------------------------------------------------*/
;*    integers                                                         */
;*---------------------------------------------------------------------*/
(define &integers
   (list->vector
      (map (lambda (i)
	      (js-ascii-name->jsstring (fixnum->string i)))
	 (iota 111 -10))))

;*---------------------------------------------------------------------*/
;*    known strings ...                                                */
;*---------------------------------------------------------------------*/
(define &empty (js-ascii->jsstring ""))
(define &__proto__ (js-ascii-name->jsstring "__proto__"))
(define &Array (js-ascii-name->jsstring "Array"))
(define &Buffer (js-ascii-name->jsstring "Buffer"))
(define &GLOBAL (js-ascii-name->jsstring "GLOBAL"))
(define &Object (js-ascii-name->jsstring "Object"))
(define &String (js-ascii-name->jsstring "String"))
(define &Worker (js-ascii-name->jsstring "Worker"))
(define &charAt (js-ascii-name->jsstring "charAt"))
(define &charCodeAt (js-ascii-name->jsstring "charCodeAt"))
(define &compiler (js-ascii-name->jsstring "compiler"))
(define &configurable (js-ascii-name->jsstring "configurable"))
(define &console (js-ascii-name->jsstring "console"))
(define &constructor (js-ascii-name->jsstring "constructor"))
(define &done (js-ascii-name->jsstring "done"))
(define &exports (js-ascii-name->jsstring "exports"))
(define &exec (js-ascii-name->jsstring "exec"))
(define &enumerable (js-ascii-name->jsstring "enumerable"))
(define &for (js-ascii-name->jsstring "for"))
(define &forEach (js-ascii-name->jsstring "forEach"))
(define &get (js-ascii-name->jsstring "get"))
(define &global (js-ascii-name->jsstring "global"))
(define &hop (js-ascii-name->jsstring "hop"))
(define &indexOf (js-ascii-name->jsstring "indexOf"))
(define &iterator (js-ascii-name->jsstring "iterator"))
(define &keyFor (js-ascii-name->jsstring "keyFor"))
(define &lastIndex (js-ascii-name->jsstring "lastIndex"))
(define &lastIndexOf (js-ascii-name->jsstring "lastIndexOf"))
(define &length (js-ascii-name->jsstring "length"))
(define &localeCompare (js-ascii-name->jsstring "localCompare"))
(define &log (js-ascii-name->jsstring "log"))
(define &match (js-ascii-name->jsstring "match"))
(define &map (js-ascii-name->jsstring "map"))
(define &module (js-ascii-name->jsstring "module"))
(define &naturalCompare (js-ascii-name->jsstring "naturalCompare"))
(define &next (js-ascii-name->jsstring "next"))
(define &prototype (js-ascii-name->jsstring "prototype"))
(define &replace (js-ascii-name->jsstring "replace"))
(define &require (js-ascii-name->jsstring "require"))
(define &return (js-ascii-name->jsstring "return"))
(define &set (js-ascii-name->jsstring "set"))
(define &slice (js-ascii-name->jsstring "slice"))
(define &split (js-ascii-name->jsstring "split"))
(define &substr (js-ascii-name->jsstring "substr"))
(define &substring (js-ascii-name->jsstring "substring"))
(define &toLowerCase (js-ascii-name->jsstring "toLowerCase"))
(define &toLocaleLowerCase (js-ascii-name->jsstring "toLocaleLowerCase"))
(define &toUpperCase (js-ascii-name->jsstring "toUpperCase"))
(define &toLocaleUpperCase (js-ascii-name->jsstring "toLocaleUpperCase"))
(define &toString (js-ascii-name->jsstring "toString"))
(define &trim (js-ascii-name->jsstring "trim"))
(define &value (js-ascii-name->jsstring "value"))
(define &valueOf (js-ascii-name->jsstring "valueOf"))
(define &writable (js-ascii-name->jsstring "writable"))


   
