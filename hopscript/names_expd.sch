;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/names_expd.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  1 08:50:34 2019                          */
;*    Last change :  Fri Apr 12 10:13:45 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript name expanders                                         */
;*    -------------------------------------------------------------    */
;*    See expanders.sch and names.sch                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    &strings ...                                                     */
;*---------------------------------------------------------------------*/
;*    '#("" "__dirname" "__filename" "__proto__"                       */
;*       "Array" "Buffer" "Error" "GLOBAL" "HEAD"                      */
;*       "Infinity" "-Infinity" "NaN" "Object"                         */
;*       "SCRIPT" "String" "Worker"                                    */
;*       "apply" "call" "callee" "caller"                              */
;*       "clearImmediate" "clearInterval" "clearTimeout"               */
;*       "console" "constructor"                                       */
;*       "default"                                                     */
;*       "exports"                                                     */
;*       "filename"                                                    */
;*       "get"                                                         */
;*       "global"                                                      */
;*       "hop"                                                         */
;*       "length"                                                      */
;*       "module"                                                      */
;*       "process"                                                     */
;*       "prototype"                                                   */
;*       "readable"                                                    */
;*       "require"                                                     */
;*       "set" "setImmediate" "setInterval" "setTimeout"               */
;*       "toString"                                                    */
;*       "value"                                                       */
;*       "write"                                                       */
;*       "writable"))                                                  */

;*---------------------------------------------------------------------*/
;*    &name-expander ...                                               */
;*---------------------------------------------------------------------*/
(define (&name-expander x e)

   (define &strings
      '#())
   
   (define (vector-index val vector)
      (let loop ((i (-fx (vector-length vector) 1)))
	 (when (>=fx i 0)
	    (if (string=? val (vector-ref vector i))
		i
		(loop (-fx i 1))))))

   (match-case x
      ((?- strings)
       `',&strings)
      
      ((?- (and ?val (? string?)))
       (cond
;* 	  ((vector-index val &strings)                                 */
;* 	   =>                                                          */
;* 	   (lambda (i)                                                 */
;* 	      `(with-access::JsGlobalObject %this (js-string-names)    */
;* 		  (vector-ref js-string-names ,i))))                   */
	  ((string->number val)
	   =>
	   (lambda (n)
	      (if (fixnum? n)
		  (vector 2 n)
		  (vector 0 val))))
	  ((eq? (string-minimal-charset val) 'ascii)
	   (vector 0 val))
	  (else
	   (vector 1 val))))
      (else
       (error "&name" "bad syntax" x))))
