;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/make_fakeuv.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar  3 18:53:45 2015                          */
;*    Last change :  Thu Sep  5 13:12:30 2019 (serrano)                */
;*    Copyright   :  2015-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Small utility to build a fake __nodejs_uv module                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module mkfakeuv
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (let loop ()
      (let ((e (read)))
	 (unless (eof-object? e)
	    (let ((f (fake e)))
	       (when f
		  (write f)
		  (newline)))
	    (loop)))))

;*---------------------------------------------------------------------*/
;*    fake ...                                                         */
;*---------------------------------------------------------------------*/
(define (fake expr)
   (match-case expr
      ((module ?- . ?-) #f)
      ((define __js_strings (&begin!)) #f)
      ((&end!) #f)
      (((or define-inline define-method define) (?id . ?-) . ?-)
       (fake-define (find-type id) expr))
      ((cond-expand (enable-libuv . ?body))
       `(cond-expand ((not enable-libuv) ,@(map fake body))))
      (else expr)))

;*---------------------------------------------------------------------*/
;*    find-type ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-type id)
   (let ((split (pregexp-match "([^:]+)::([^:]+)" (symbol->string id))))
      (if (pair? split) (string->symbol (caddr split)) 'obj)))
   
;*---------------------------------------------------------------------*/
;*    fake-define ...                                                  */
;*---------------------------------------------------------------------*/
(define (fake-define typ expr)
   (match-case expr
      ((?kwd (nodejs-guess-handle-type . ?args) . ?body)
       `(,kwd (nodejs-guess-handle-type ,@args)
	   (js-string->jsstring "TTY")))
      ((?kwd ?proto . ?body)
       `(,kwd ,proto ,(make-body typ)))
      (else
       expr)))

;*---------------------------------------------------------------------*/
;*    make-body ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-body type)
   (case type
      ((obj) #unspecified)
      ((bstring) "")
      ((JsStringLiteral) '(js-string->jsstring ""))
      ((uint64) #u64:0)
      ((double) 0.0)
      ((long) 0)
      ((bool) #f)
      ((vector) ''#())
      (else 0)))

   
	 
