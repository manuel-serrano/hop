;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/runtime/bglcpp.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 31 13:25:35 2013                          */
;*    Last change :  Wed Aug 14 07:29:03 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    A minimalist CPP-like tool to generate the correct scheme2js     */
;*    runtime file.                                                    */
;*    -------------------------------------------------------------    */
;*    It only supports #define, #if, #else, and #endif commands.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglcpp
   (main main))

;*---------------------------------------------------------------------*/
;*    props ...                                                        */
;*---------------------------------------------------------------------*/
(define props '())

;*---------------------------------------------------------------------*/
;*    srcs ...                                                         */
;*---------------------------------------------------------------------*/
(define srcs '())

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main x)
   (args-parse (cdr x)
      (("-D?prop" (help "Define bglcpp property"))
       (set! props (cons prop props)))
      (else
       (set! srcs (cons else srcs))))
   (if (null? srcs)
       (cpp (current-input-port))
       (for-each (lambda (f) (call-with-input-file f cpp)) srcs))
   0)

;*---------------------------------------------------------------------*/
;*    cpp ...                                                          */
;*---------------------------------------------------------------------*/
(define (cpp p)

   (define (all-true? state)
      (every (lambda (x) x) state))
   
   (let loop ((state (list #t)))
      (let ((line (read-line-newline p)))
	 (unless (eof-object? line)
	    (cond
	       ((string-prefix? "#define " line)
		(newline)
		(when (all-true? state)
		   (let* ((i (string-skip line #\space 8))
			  (j (string-index line "\n\r\t " i)))
		      (set! props (cons (substring line i j) props))))
		(loop state))
	       ((string-prefix? "#if " line)
		(newline)
		(let* ((i (string-skip line #\space 4))
		       (j (string-index line "\n\r\t" i))
		       (s (substring line i j)))
		   (if (char=? (string-ref s 0) #\!)
		       (let ((as (substring line (+fx i 1) j)))
			  (loop (cons (not (member as props)) state)))
		       (loop (cons (member s props) state)))))
	       ((string-prefix? "#else" line)
		(newline)
		(loop (cons (not (car state)) (cdr state))))
	       ((string-prefix? "#endif" line)
		(newline)
		(loop (cdr state)))
	       (else
		(when (all-true? state) (display line))
		(loop state)))))))
