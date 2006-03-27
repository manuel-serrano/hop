;*=====================================================================*/
;*    serrano/prgm/project/hop/demos/widgets/sudoku/sudoku.jscm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Wed Mar 22 14:27:22 2006                          */
;*    Last change :  Sun Mar 26 20:32:15 2006 (eg)                     */
;*    -------------------------------------------------------------    */
;*    Sudoku HOP demo                                                  */
;*=====================================================================*/

(define *selected-cell* #f)
;(define *selected-col*  0)
;(define *selected-row* 0)
(define *board* #f)

(define (board-ref i j)
  (vector-ref (vector-ref j) i))




(define (set-cell-background! cell bg)
  (let ((parent cell.parentNode))
    (set! parent.style.background bg)))

(define (set-cell-foreground! cell fg)
  (set! cell.style.color fg))



(define (find-cell-value cell)
  (let* ((tmp (cell.id.split "-")) ;; id is "sudoku-row-col"
	 (row (vector-ref tmp 1))
	 (col (vector-ref tmp 2)))
    (vector-ref (vector-ref *board* row) col)))

(define (set-cell-value! cell v)
  (set! cell.innerHTML v)
  (set! cell.style.display "block"))


(define (click-cell id)
  (let ((el (document.getElementById id)))
    (if  *selected-cell*
	(set-cell-background! *selected-cell* ""))
    (set-cell-background! el "#e0ffff")
    (set! *selected-cell* el)))


(define (key-event e)
  (let ((key-code e.which))
    (case key-code
      ((49 50 51 52 53 54 55 56 57	  	;; A digit
	97 98 99 100 101 102 103 104 105)	;; A digit (keypad)
         (let ((val (find-cell-value *selected-cell*))
	       (in  (- key-code (if (>= key-code 97) 96 48))))
	   (if (positive? val)
	       (begin
		 (set-cell-value! *selected-cell* in)
		 (set-cell-foreground! *selected-cell*
				       (if (= val in) "black" "red"))))))
      ((16) #t)
      ((191 72) 				;; ? or h: hint
       (set-cell-value! *selected-cell* (find-cell-value *selected-cell*))
       (set-cell-foreground! *selected-cell* "#7EF09E"))
      ((80)					;; p: print
       (window.print))       
      (else (alert key-code)))))

		   

