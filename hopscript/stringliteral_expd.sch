;*=====================================================================*/
;*    .../prgm/project/hop/hop/hopscript/stringliteral_expd.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 24 02:21:25 2017                          */
;*    Last change :  Wed Mar 11 08:57:54 2020 (serrano)                */
;*    Copyright   :  2017-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    string expanders                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-jsstring-charat ...                                           */
;*---------------------------------------------------------------------*/
(define-macro (js-jsstring-charat this position %this)
   (cond
      ((and (fixnum? position) (>=fx position 0))
       `(let ((%s ,this))
	   (if (and (js-jsstring-ascii? %s) (js-jsstring-normalized? %s))
	       (with-access::JsStringLiteral %s (left)
		  (let ((%val left))
		     (if (<fx ,position (string-length %val))
			 (with-access::JsGlobalObject ,%this (char-table)
			    (let* ((c (string-ref-ur %val ,position))
				   (i (char->integer c)))
			       (vector-ref char-table i)))
			 (& ""))))
	       ((@ js-jsstring-charat __hopscript_stringliteral)
		%s ,position ,%this))))
      ((fixnum? position)
       (& ""))
      (else
       `(let ((%s ,this)
	      (%p ,position))
	   (if (and (js-jsstring-ascii? %s) (js-jsstring-normalized? %s)
		    (fixnum? %p))
	       (with-access::JsStringLiteral %s (left)
		  (let ((%val left))
		     (if (and (>=fx %p 0) (<fx %p (string-length %val)))
			 (with-access::JsGlobalObject ,%this (char-table)
			    (let* ((c (string-ref-ur %val %p))
				   (i (char->integer c)))
			       (vector-ref char-table i)))
			 (& ""))))
	       ((@ js-jsstring-charat __hopscript_stringliteral)
		%s %p ,%this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-charcodeat ...                                       */
;*---------------------------------------------------------------------*/
(define-macro (js-jsstring-charcodeat this position %this)
   (cond
      ((and (fixnum? position) (>=fx position 0))
       `(let ((%s ,this))
	   (if (and (js-jsstring-ascii? %s) (js-jsstring-normalized? %s))
	       (with-access::JsStringLiteral %s (left)
		  (let ((%val left))
		     (if (<fx ,position (string-length %val))
			 (char->integer (string-ref-ur %val ,position))
			 +nan.0)))
	       ((@ js-jsstring-charcodeat __hopscript_stringliteral)
		%s ,position ,%this))))
      ((fixnum? position)
       +nan.0)
      (else
       `(let ((%s ,this)
	      (%p ,position))
	   (if (and (js-jsstring-ascii? %s) (js-jsstring-normalized? %s)
		    (fixnum? %p))
	       (with-access::JsStringLiteral %s (left)
		  (let ((%val left))
		     (if (and (>=fx %p 0) (<fx %p (string-length %val)))
			 (char->integer (string-ref-ur %val %p))
			 +nan.0)))
	       ((@ js-jsstring-charcodeat __hopscript_stringliteral)
		%s %p ,%this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-codepointat ...                                      */
;*---------------------------------------------------------------------*/
(define-macro (js-jsstring-codepointat this position %this)
   (cond
      ((and (fixnum? position) (>=fx position 0))
       `(let ((%s ,this))
	   (if (and (js-jsstring-ascii? %s) (js-jsstring-normalized? %s))
	       (with-access::JsStringLiteral %s (left)
		  (let ((%val left))
		     (if (<fx ,position (string-length %val))
			 (char->integer (string-ref-ur %val ,position))
			 +nan.0)))
	       ((@ js-jsstring-codepointat __hopscript_stringliteral)
		%s ,position ,%this))))
      ((fixnum? position)
       +nan.0)
      (else
       `(let ((%s ,this)
	      (%p ,position))
	   (if (and (js-jsstring-ascii? %s) (js-jsstring-normalized? %s)
		    (fixnum? %p))
	       (with-access::JsStringLiteral %s (left)
		  (let ((%val left))
		     (if (and (>=fx %p 0) (<fx %p (string-length %val)))
			 (char->integer (string-ref-ur %val %p))
			 +nan.0)))
	       ((@ js-jsstring-codepointat __hopscript_stringliteral)
		%s %p ,%this))))))

;*---------------------------------------------------------------------*/
;*    js-jsstring->string ...                                          */
;*---------------------------------------------------------------------*/
(define-macro (js-jsstring->string str)
   (match-case str
      ((& (and ?val (? string?)))
       val)
      (else
       `((@ js-jsstring->string __hopscript_stringliteral) ,str))))

;*---------------------------------------------------------------------*/
;*    js-jsstring-codeunit-length ...                                  */
;*---------------------------------------------------------------------*/
(define-expander js-jsstring-codeunit-length
   (lambda (x e)
      (match-case x
	 ((js-jsstring-codeunit-length (and ?js (? symbol?)))
	  (e `(if (js-jsstring-ascii? ,js)
		  (with-access::JsStringLiteralASCII ,js (length)
		     length)
		  ((@ js-jsstring-codeunit-length __hopscript_stringliteral) ,js))
	     e))
	 ((js-jsstring-codeunit-length ?expr)
	  (let ((js (gensym)))
	     (e `(let ((,js ,expr)) (js-jsstring-codeunit-length ,js)) e)))
	 (else
	  `((@ js-jsstring-codeunit-length __hopscript_stringliteral)
	    ,(map (lambda (x) (e x e)) (cdr x)))))))
