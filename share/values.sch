;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/share/values.sch                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Aug  7 10:21:45 2018                          */
;*    Last change :  Tue Aug  7 10:24:50 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme2JS does not support properly tailrec MULTIPLE-VALUE-BIND. */
;*    This causes the vlq-base64 decode to crash on Chromium           */
;*    (as of 2018). These macros workaround the problem by using       */
;*    implementing tailrec versions. They assume mono-threaded         */
;*    executions.                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    values ...                                                       */
;*    -------------------------------------------------------------    */
;*    ad hoc (and hacky) macros to override scheme2js                  */
;*    muliple-value-bind/values compilation scheme which is not        */
;*    tailrec and that causes the decoding of large source-map         */
;*    files to crash on chromium (2018 version) that apparently is     */
;*    not properly tailrec.                                            */
;*    -------------------------------------------------------------    */
;*    This assumes that JS is mono-threaded.                           */
;*---------------------------------------------------------------------*/
(define-macro (values . l)
   `(set! *values* (vector ,@l)))

;*---------------------------------------------------------------------*/
;*    multiple-value-bind ...                                          */
;*---------------------------------------------------------------------*/
(define-macro (multiple-value-bind bindings expr . body)
   `(begin
       ,expr
       (let ,(map (lambda (var idx)
		     `(,var (vector-ref *values* ,idx)))
		bindings (iota (length bindings)))
	  ,@body)))

;*---------------------------------------------------------------------*/
;*    *values* ...                                                     */
;*---------------------------------------------------------------------*/
(define *values* '())


