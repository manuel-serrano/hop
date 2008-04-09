;*=====================================================================*/
;*    .../project/hop/1.9.x/weblets/home/demos/sudoku/sudoku.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Wed Mar 22 14:27:22 2006                          */
;*    Last change :  Tue Dec  4 16:08:41 2007 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Sudoku HOP demo                                                  */
;*=====================================================================*/
(module sudoku
   (export *show-errors*
	   show-solution
	   click-cell
	   start-game)
   (JS document))

(define *selected-cell* #f)
(define *board* #f) ;; will be set in start-game
(define *show-errors* #t)


(define (board-ref i j)
  (vector-ref (vector-ref *board* j) i))




(define (set-cell-background! cell bg)
   (let ((parent (dom-parent-node cell)))
      (node-style-set! parent "background" bg)))

(define (set-cell-foreground! cell fg)
   (node-style-set! cell "color" fg))



(define (find-cell-value cell)
  (let* ((tmp (cell.id.split "-")) ;; id is "sudoku-row-col"
	 (row (vector-ref tmp 1))
	 (col (vector-ref tmp 2)))
    (vector-ref (vector-ref *board* row) col)))

(define (set-cell-value! cell v)
  (innerHTML-set! cell v)
  (node-style-set! cell "display" "block"))


(define (click-cell id)
  (let ((el (dom-get-element-by-id id)))
    (if  *selected-cell*
	 (set-cell-background! *selected-cell* ""))
    (set-cell-background! el "#e0ffff")
    (set! *selected-cell* el)))


(define (key-event e)
  (let ((key-code (event-key-code e)))
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
      ((191 72) 				;; ? or h: hint
       (set-cell-value! *selected-cell* (find-cell-value *selected-cell*))
       (set-cell-foreground! *selected-cell* "#7EF09E"))
      (else #f))))

		   

;; ======================================================================
;; 	show-solution ...
;; ======================================================================
(define (show-solution)
  ;; Embedded do seems to not work for now, so use a for-each
  (for-each (lambda (i)
	      (for-each (lambda(j)
			  (let* ((id   (+ "sudoku-" i "-" j))
				 (cell (dom-get-element-by-id id))
				 (val (find-cell-value cell)))
			    (if (> val 0)
				(set-cell-value! cell val))))
			'(0 1 2 3 4 5 6 7 8)))
	    '(0 1 2 3 4 5 6 7 8)))

;; ======================================================================
;; 	start-game
;; ======================================================================
(define (start-game board)
   (set! *board* board)
   (add-event-listener! document "keydown" key-event))
