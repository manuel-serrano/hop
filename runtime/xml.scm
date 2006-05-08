;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/xml.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  8 05:43:46 2004                          */
;*    Last change :  Mon May  8 11:29:03 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple XML producer/writer for HOP.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_xml

   (library web)
   
   (include "eval-macro.sch"
	    "xml.sch")

   (import  __hop_types
	    __hop_mime
	    __hop_misc)
   
   (export  (class xml
	       (%xml-constructor)
	       (id::bstring read-only (default "_")))

	    (class css::xml)
	    
	    (class xml-if::xml
	       (test::procedure read-only)
	       (then read-only)
	       (otherwise read-only))
	    
	    (class xml-ghost::xml
	       (body read-only))
	    
	    (class xml-delay::xml
	       (thunk::procedure read-only)
	       (value::obj (default #f)))

	    (class xml-markup::xml
	       (markup::symbol read-only)
	       (attributes::pair-nil (default '()))
	       body::pair-nil)

	    (class xml-element::xml-markup
	       (parent (default #unspecified)))

	    (class xml-html::xml-markup
	       (prelude read-only (default "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">")))

	    (generic %xml-constructor ::xml)
	    (xml-constructor-add! ::symbol ::procedure)
	    (%make-xml-element ::symbol ::pair-nil)

	    (xml-markup-is? ::obj ::symbol)

	    (xml-make-id::bstring #!optional id (markup 'HOP))
	    
 	    (generic xml-write ::obj ::output-port ::symbol)
	    (generic xml-write-attribute attr::obj id p)

	    (string->html ::bstring)
	    (string->xml ::bstring)
	    
	    (<A> . ::obj)
	    (<ABBR> . ::obj)
	    (<ACRONYM> . ::obj)
	    (<ADDRESS> . ::obj)
	    (<APPLET> . ::obj)
	    (<AREA> . ::obj)
	    (<B> . ::obj)
	    (<BASE> . ::obj)
	    (<BASEFONT> . ::obj)
	    (<BDO> . ::obj)
	    (<BIG> . ::obj)
	    (<BLOCKQUOTE> . ::obj)
	    (<BODY> . ::obj)
	    (<BR> . ::obj)
	    (<BUTTON> . ::obj)
	    (<CANVAS> . ::obj)
	    (<CAPTION> . ::obj)
	    (<CENTER> . ::obj)
	    (<CITE> . ::obj)
	    (<CODE> . ::obj)
	    (<COL> . ::obj)
	    (<COLGROUP> . ::obj)
	    (<DD> . ::obj)
	    (<DEL> . ::obj)
	    (<DFN> . ::obj)
	    (<DIR> . ::obj)
 	    (<DIV> . ::obj)
	    (<DL> . ::obj)
	    (<DT> . ::obj)
	    (<EM> . ::obj)
	    (<FIELDSET> . ::obj)
	    (<FONT> . ::obj)
	    (<FORM> . ::obj)
	    (<FRAME> . ::obj)
	    (<FRAMESET> . ::obj)
	    (<H1> . ::obj)
	    (<H2> . ::obj)
	    (<H3> . ::obj)
	    (<H4> . ::obj)
	    (<H5> . ::obj)
	    (<H6> . ::obj)
	    (<HR> . ::obj)
	    (<HTML> . ::obj)
	    (<I> . ::obj)
	    (<IFRAME> . ::obj)
	    (<IMG> . ::obj)
	    (<INPUT> . ::obj)
	    (<INS> . ::obj)
	    (<ISINDEX> . ::obj)
	    (<KBD> . ::obj)
	    (<LABEL> . ::obj)
	    (<LEGEND> . ::obj)
	    (<LI> . ::obj)
	    (<LINK> . ::obj)
	    (<MAP> . ::obj)
	    (<MARQUEE> . ::obj)
	    (<MENU> . ::obj)
	    (<META> . ::obj)
	    (<NOFRAMES> . ::obj)
	    (<NOSCRIPT> . ::obj)
	    (<OBJECT> . ::obj)
	    (<OL> . ::obj)
	    (<OPTGROUP> . ::obj)
	    (<OPTION> . ::obj)
	    (<P> . ::obj)
	    (<PARAM> . ::obj)
	    (<PRE> . ::obj)
	    (<Q> . ::obj)
	    (<S> . ::obj)
	    (<SAMP> . ::obj)
	    (<SCRIPT> . ::obj)
	    (<SELECT> . ::obj)
	    (<SMALL> . ::obj)
	    (<SPAN> . ::obj)
	    (<STRIKE> . ::obj)
	    (<STRONG> . ::obj)
	    (<STYLE> . ::obj)
	    (<SUB> . ::obj)
	    (<SUP> . ::obj)
	    (<TABLE> . ::obj)
	    (<TBODY> . ::obj)
	    (<TD> . ::obj)
	    (<TEXTAREA> . ::obj)
	    (<TFOOT> . ::obj)
	    (<TH> . ::obj)
	    (<THEAD> . ::obj)
	    (<TITLE> . ::obj)
	    (<TR> . ::obj)
	    (<TT> . ::obj)
	    (<U> . ::obj)
	    (<UL> . ::obj)
	    (<VAR> . ::obj)
	    
	    (<DELAY> . ::obj)
	    (<GHOST> . ::obj)))

;*---------------------------------------------------------------------*/
;*    *xml-constructors* ...                                           */
;*---------------------------------------------------------------------*/
(define *xml-constructors* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    xml-constructor-add! ...                                         */
;*---------------------------------------------------------------------*/
(define (xml-constructor-add! id proc)
   (if (not (correct-arity? proc 1))
       (error 'xml-constructor-add! "Illegal constructor" proc)
       (hashtable-put! *xml-constructors* id proc)))

;*---------------------------------------------------------------------*/
;*    %xml-constructor ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (%xml-constructor o::xml)
   (with-access::xml o (id)
      (let ((hook (hashtable-get *xml-constructors* id)))
	 (when (procedure? hook)
	    (hook o)))
      o))

;*---------------------------------------------------------------------*/
;*    %xml-constructor ...                                             */
;*---------------------------------------------------------------------*/
(define-method (%xml-constructor o::xml-markup)
   (call-next-method)
   (with-access::xml-markup o (body markup)
      (let loop ((es body))
	 (cond
	    ((pair? es)
	     (let ((e (car es)))
		(cond
		   ((xml-element? e)
		    (xml-element-parent-set! e o))
		   ((pair? e)
		    (loop e))))
	     (loop (cdr es)))
	    ((xml-element? es)
	     (xml-element-parent-set! es o))))
      o))

;*---------------------------------------------------------------------*/
;*    %make-xml-element ...                                            */
;*---------------------------------------------------------------------*/
(define (%make-xml-element el args)
   (let loop ((args args)
	      (attr '())
	      (body '())
	      (id #unspecified))
      (cond
	 ((null? args)
	  (instantiate::xml-element
	     (markup (string->symbol
		      (string-downcase
		       (symbol->string el))))
	     (attributes (reverse! attr))
	     (id (xml-make-id id el))
	     (body (reverse! body))))
	 ((keyword? (car args))
	  (if (null? (cdr args))
	      (error (symbol-append '< el '>)
		     "attribute value missing"
		     (car args))
	      (if (eq? (car args) :id)
		  (if (string? (cadr args))
		      (loop (cddr args) attr body (cadr args))
		      (bigloo-type-error el "string" (cadr args)))
		  (loop (cddr args)
			(cons (cons (keyword->string (car args)) (cadr args))
			      attr)
			body
			id))))
	 ((null? (car args))
	  (loop (cdr args) attr body id))
	 ((pair? (car args))
	  (loop (append (car args) (cdr args)) attr body id))
	 (else
	  (loop (cdr args) attr (cons (car args) body) id)))))

;*---------------------------------------------------------------------*/
;*    xml-markup-is? ...                                               */
;*---------------------------------------------------------------------*/
(define (xml-markup-is? o markup)
   (and (xml-markup? o) (eq? (xml-markup-markup o) markup)))

;*---------------------------------------------------------------------*/
;*    xml-make-id ...                                                  */
;*---------------------------------------------------------------------*/
(define (xml-make-id #!optional id (markup 'HOP))
   (if (string? id)
       id
       (symbol->string (gensym markup))))

;*---------------------------------------------------------------------*/
;*    xml-write ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (xml-write obj p encoding)
   (cond
      ((string? obj)
       (if (eq? encoding 'UTF-8)
	   (display (iso-latin->utf8! obj) p)
	   (display obj p)))
      ((or (number? obj) (symbol? obj))
       (display obj p))
      ((pair? obj)
       (for-each (lambda (o) (xml-write o p encoding)) obj))
      ((date? obj)
       (display obj p))
      ((null? obj)
       #unspecified)
      ((eq? obj #unspecified)
       #unspecified)
      ((eq? obj #f)
       #unspecified)
      ((eq? obj #t)
       #unspecified)
      (else
       (error 'xml-write "Illegal xml object" obj))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-if ...                                           */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-if p encoding)
   (with-access::xml-if obj (test then otherwise)
      (if (test)
	  (xml-write then p encoding)
	  (xml-write otherwise p encoding))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-ghost ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-ghost p encoding)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-delay ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-delay p encoding)
   (with-access::xml-delay obj (thunk value)
      (unless value (set! value (thunk)))
      (xml-write value p encoding)))

;*---------------------------------------------------------------------*/
;*    xml-write ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-markup p encoding)
   (with-access::xml-element obj (markup attributes body)
      (when (pair? body)
	 (display "<" p)
	 (display markup p)
	 (xml-write-attributes attributes p)
	 (display ">" p)
	 (for-each (lambda (b) (xml-write b p encoding)) body)
	 (display "</" p)
	 (display markup p)
	 (display ">" p))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-element ...                                      */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-element p encoding)
   (with-access::xml-element obj (markup id attributes body)
      (if (and (null? body)
	       (null? attributes)
	       (memq markup '(br img input)))
	  (begin
	     (display "<" p)
	     (display markup p)
	     (display " id=\"" p)
	     (display id p)
	     (display "\"/>" p))
	  (begin
	     (display "<" p)
	     (display markup p)
	     (display " id=\"" p)
	     (display id p)
	     (display "\"" p)
	     (xml-write-attributes attributes p)
	     (display ">" p)
	     (for-each (lambda (b) (xml-write b p encoding)) body)
	     (display "</" p)
	     (display markup p)
	     (display ">" p)))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-html ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-html p encoding)
   (with-access::xml-html obj (prelude)
      (when prelude (display prelude p))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    xml-write-attributes ...                                         */
;*---------------------------------------------------------------------*/
(define (xml-write-attributes attr p)
   (for-each (lambda (a)
		(display " " p)
		(xml-write-attribute (cdr a) (car a) p))
	     attr))

;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::obj ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (xml-write-attribute attr::obj id p)
   ;; boolean false attribute has no value
   (unless (eq? attr #f)
      (display id p)
      ;; boolean true attribute has no value
      (unless (eq? attr #t)
	 (display "='" p)
	 (if (procedure? attr)
	     (error 'xml "Illegal procedure argument in XML attribute" id)
	     (display (xml-attribute-encode attr) p))
	 (display "'" p))))

;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::hop-service ...                            */
;*---------------------------------------------------------------------*/
(define-method (xml-write-attribute attr::hop-service id p)
   (display id p)
   (display "='" p)
   (display (hop-service-path attr) p)
   (display "'" p))

;*---------------------------------------------------------------------*/
;*    string->html ...                                                 */
;*---------------------------------------------------------------------*/
(define (string->html h)
   (with-input-from-string h
      (lambda ()
	 (car (html-parse
	       (current-input-port)
	       0
	       (lambda (markup attributes body)
		  (let* ((m (string->symbol
			     (string-append
			      "<"
			      (string-upcase (symbol->string markup))
			      ">")))
			 (a (append-map (lambda (a)
					   (list (symbol->keyword (car a))
						 (cdr a)))
					attributes))
			 (e `(,m ,@a ,@body)))
		     (eval e))))))))

;*---------------------------------------------------------------------*/
;*    string->xml ...                                                  */
;*---------------------------------------------------------------------*/
(define (string->xml h)
   (with-input-from-string h
      (lambda ()
	 (car (xml-parse
	       (current-input-port)
	       0
	       (lambda (markup attributes body)
		  (let* ((m (string->symbol
			     (string-append
			      "<"
			      (string-upcase (symbol->string markup))
			      ">")))
			 (a (append-map (lambda (a)
					   (list (symbol->keyword (car a))
						 (cdr a)))
					attributes))
			 (e `(,m ,@a ,@body)))
		     (eval e))))))))

;*---------------------------------------------------------------------*/
;*    HTML 4.01 elements ...                                           */
;*---------------------------------------------------------------------*/
(define-xml-element <A>)
(define-xml-element <ABBR>)
(define-xml-element <ACRONYM>)
(define-xml-element <ADDRESS>)
(define-xml-element <APPLET>)
(define-xml-element <AREA>)
(define-xml-element <B>)
(define-xml-element <BASE>)
(define-xml-element <BASEFONT>)
(define-xml-element <BDO>)
(define-xml-element <BIG>)
(define-xml-element <BLOCKQUOTE>)
(define-xml-element <BODY>)
(define-xml-element <BR>)
(define-xml-element <BUTTON>)
(define-xml-element <CANVAS>)
(define-xml-element <CAPTION>)
(define-xml-element <CENTER>)
(define-xml-element <CITE>)
(define-xml-element <CODE>)
(define-xml-element <COL>)
(define-xml-element <COLGROUP>)
(define-xml-element <DD>)
(define-xml-element <DEL>)
(define-xml-element <DFN>)
(define-xml-element <DIR>)
(define-xml-element <DIV>)
(define-xml-element <DL>)
(define-xml-element <DT>)
(define-xml-element <EM>)
(define-xml-element <FIELDSET>)
(define-xml-element <FONT>)
(define-xml-element <FORM>)
(define-xml-element <FRAME>)
(define-xml-element <FRAMESET>)
(define-xml-element <H1>)
(define-xml-element <H2>)
(define-xml-element <H3>)
(define-xml-element <H4>)
(define-xml-element <H5>)
(define-xml-element <H6>)
(define-xml-element <HR>)
(define-xml xml-html <HTML>)
(define-xml-element <I>)
(define-xml-element <IFRAME>)
(define-xml-element <INPUT>)
(define-xml-element <INS>)
(define-xml-element <ISINDEX>)
(define-xml-element <KBD>)
(define-xml-element <LABEL>)
(define-xml-element <LEGEND>)
(define-xml-element <LI>)
(define-xml-element <LINK>)
(define-xml-element <MAP>)
(define-xml-element <MARQUEE>)
(define-xml-element <MENU>)
(define-xml-element <META>)
(define-xml-element <NOFRAMES>)
(define-xml-element <NOSCRIPT>)
(define-xml-element <OBJECT>)
(define-xml-element <OL>)
(define-xml-element <OPTGROUP>)
(define-xml-element <OPTION>)
(define-xml-element <P>)
(define-xml-element <PARAM>)
(define-xml-element <PRE>)
(define-xml-element <Q>)
(define-xml-element <S>)
(define-xml-element <SAMP>)
(define-xml-element <SCRIPT>)
(define-xml-element <SELECT>)
(define-xml-element <SMALL>)
(define-xml-element <SPAN>)
(define-xml-element <STRIKE>)
(define-xml-element <STRONG>)
(define-xml xml-markup <STYLE>)
(define-xml-element <SUB>)
(define-xml-element <SUP>)
(define-xml-element <TABLE>)
(define-xml-element <TBODY>)
(define-xml-element <TD>)
(define-xml-element <TEXTAREA>)
(define-xml-element <TFOOT>)
(define-xml-element <TH>)
(define-xml-element <THEAD>)
(define-xml xml-markup <TITLE>)
(define-xml-element <TR>)
(define-xml-element <TT>)
(define-xml-element <U>)
(define-xml-element <UL>)
(define-xml-element <VAR>)

;*---------------------------------------------------------------------*/
;*    <DELAY> ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <DELAY> ((id #unspecified string)
			      body)
   (if (and (pair? body)
	    (procedure? (car body))
	    (correct-arity? (car body) 0))
       (instantiate::xml-delay
	  (id (xml-make-id id 'DELAY))
	  (thunk (car body)))
       (error '<DELAY> "Illegal delay's thunk" (car body))))

;*---------------------------------------------------------------------*/
;*    <GHOST> ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <GHOST> ((id #unspecified string)
			      body)
   (cond
      ((null? body)
       (error '<GHOST> "Illegal empty ghost body" body))
      ((pair? (cdr body))
       (instantiate::xml-ghost
	  (id (xml-make-id id 'GHOST))
	  (body body)))
      (else
       (instantiate::xml-ghost
	  (id (xml-make-id id 'GHOST))
	  (body (car body))))))

;*---------------------------------------------------------------------*/
;*    IMG ...                                                          */
;*---------------------------------------------------------------------*/
(define-xml-compound <IMG> ((id #unspecified string)
			    (inline #f boolean)
			    (src #unspecified string)
			    (attributes)
			    body)
   (if (not (string? src))
       (error '<IMG> "Illegal image src" src)
       (let ((src (if (and inline (file-exists? src))
		      (let ((p (open-input-file src)))
			 (if (input-port? p)
			     (unwind-protect
				(format "data:~a;base64,~a"
					(mime-type src (format "image/~a"
							       (suffix src)))
					(base64-encode (read-string p)))
				(close-input-port p))
			     src))
		      src)))
	  (instantiate::xml-element
	     (markup 'img)
	     (id (xml-make-id id 'img))
	     (attributes (cons `(src . ,src) attributes))
	     (body body)))))
	  

