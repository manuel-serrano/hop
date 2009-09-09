;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module transform-util
   (import nodes
	   error
	   export-desc
	   symbol)
   (export (parameter-assig-mapping operands
				    formals
				    vaarg)))

;; needed for inlining, ...
;; if vaarg? is #t then the last element is a vaarg.
;;
;; given a call to a function with signature (formals . vaarg)
;; this function returns a list of pairs (formal . value).
;; Ex: (parameter-assig-mapping ((+ 1 1) 2 3 4) (x y) (z)
;;   => ((x (+ 1 1)) (y 2) (z (list 3 4)))
;; The formals/vaarg are *not* referenced.
;;
;; Ensures that the order of assignment is that of formals
(define (parameter-assig-mapping operands formals vaarg?)
   (let loop ((opnds operands)
	      (formals formals)
	      (rev-res '()))
      (cond
	 ;; nothing left to do.
	 ((and (null? opnds)
	       (null? formals))
	  (reverse! rev-res))
	 
	 ;; no formals, but opnds
	 ((and (not (null? opnds))
	       (null? formals))
	  (scheme2js-error "parametr-assig-mapping"
			   "too many arguments"
			   '()
			   (car opnds)))
	 
	 ;; the last element is a vaarg
	 ;; and no operands left
	 ((and (null? (cdr formals))
	       vaarg?
	       (null? opnds))
	  ;; just map vaarg to '(), and return the
	  ;; whole assig-list
	  (cons (cons (car formals) (instantiate::Const (value '())))
		rev-res))
	 
	 ;; the last element is a vaarg, and there are still operands
	 ((and (null? (cdr formals))
	       vaarg?)
	  ;; create a list, and map vaarg to it.
	  ;; then return the whole list of pairs.
	  (let ((rvalue (instantiate::Call
			   (location (if (and (pair? operands)
					      (Node? (car operands)))
					 (Node-location (car operands))
					 #f))
			   (operator (runtime-reference 'list))
			   (operands opnds))))
	     (cons (cons (car formals) rvalue)
		   rev-res)))

	 ;; no opnds, but formals
	 ((and (null? opnds)
	       (not (null? formals)))
	  (scheme2js-error #f "not enough arguments"
			   (if (Ref? (car formals))
			       (Var-id (Ref-var (car formals)))
			       '())
			   (car formals)))

	 ;; still (non-vaarg)-formals and operands left.
	 (else
	  (loop (cdr opnds)
		(cdr formals)
		(cons (cons (car formals) (car opnds))
		      rev-res))))))
