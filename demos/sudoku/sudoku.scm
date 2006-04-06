;*=====================================================================*/
;*    serrano/prgm/project/hop/demos/widgets/sudoku/sudoku.jscm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Wed Mar 22 14:27:22 2006                          */
;*    Last change :  Thu Mar 30 10:53:18 2006 (eg)                     */
;*    -------------------------------------------------------------    */
;*    Sudoku HOP demo                                                  */
;*=====================================================================*/

(define *selected-cell* #f)
(define *board* #f)
(define *show-errors* #t)
(define *level* 1)

(define (board-ref i j)
  (vector-ref (vector-ref *board* j) i))

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


(define (empty-cell? cell)
  (= cell.innerHTML.length 0))


(define (select-cell cell)
  (if  *selected-cell*
      (set-cell-background! *selected-cell* ""))
  (set-cell-background! cell "#e0ffff")
  (set! *selected-cell* cell))

(define (click-cell id)
  (select-cell (document.getElementById id)))


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
				       (if (or (not *show-errors*)
					       (= val in))
					   "black"
					   "red"))))))
      ((16) #t)
      ((8 46)					;; Backspace Delete
       (set-cell-value! *selected-cell* ""))
      ((191 72) 				;; ? or h: hint
       (set-cell-value! *selected-cell* (find-cell-value *selected-cell*))
       (set-cell-foreground! *selected-cell* "#7EF09E"))
      ((37 39)					;; Left and Right arrows
       (let* ((tmp (*selected-cell*.id.split "-"))
	      (row (vector-ref tmp 1))
	      (col (vector-ref tmp 2))
	      (+/- (if (= key-code 37) - +)))
	 (let Loop ((col (+/- (/ col 1) 1)))
	   (let ((cell (document.getElementById (+ "sudoku-" row "-" col))))
	     (if (and (>= col 0) (< col 9))
		 (if (empty-cell? cell)
		     (select-cell cell)
		     (Loop (+/- col 1))))))))
      ((38 40)					;; Up and Down arrows
       (let* ((tmp (*selected-cell*.id.split "-"))
	      (row (vector-ref tmp 1))
	      (col (vector-ref tmp 2))
	      (+/- (if (= key-code 38) - +)))
	 (let Loop ((row (+/- (/ row 1) 1)))
	   (let ((cell (document.getElementById (+ "sudoku-" row "-" col))))
	     (if (and (>= row 0) (< row 9))
		 (if (empty-cell? cell)
		     (select-cell cell)
		     (Loop (+/- row 1))))))))))
  ;; Don't propagate event
  #f)

		   

(define (fill-board empty?)
  ;; Embedded DO seems to not work for now, so use a FOR-EACH
  (for-each (lambda (i)
	      (for-each (lambda(j)
			  (let* ((id   (+ "sudoku-" i "-" j))
				 (cell (document.getElementById id))
				 (val (find-cell-value cell)))
			    (if (> val 0)
				(set-cell-value! cell (if empty? "" val)))))
			'(0 1 2 3 4 5 6 7 8)))
	    '(0 1 2 3 4 5 6 7 8)))

;; ======================================================================
;; 	show-solution ...
;; ======================================================================
(define (show-solution)
  (fill-board #f))

;; ======================================================================
;; 	restart-current-game ...
;; ======================================================================
(define (restart-current-game)
  (fill-board #t))
