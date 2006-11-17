(define (img-path file)
   file)

(define *game-over* #f)

(define *width* 15)
(define *height* 15)
(define *nb-mines* 20)
(define *missing-cells* 0)
(define *nb-marked-bombs* 0)


(define *blank-img* (img-path "blank.gif"))
(define *revealed-imgs* (make-vector 8))
(let loop ((i 0))
   (if (< i 8)
       (begin
	  (vector-set! *revealed-imgs* i
		       (img-path (string-append "revealed"
						(number->string i)
						".gif")))
	  (loop (+ i 1)))))

(define *bomb-flagged-img* (img-path "bombflagged.gif"))
(define *bomb-revealed-img* (img-path "bombrevealed.gif"))
(define *bomb-misflagged-img* (img-path "bombmisflagged.gif"))
(define *bomb-death-img* (img-path "bombdeath.gif"))

(define (remaining-bombs-update!)
   (set! (document.getElementById "remaining").innerHTML (- *nb-mines* *nb-marked-bombs*)))
   
(define (cell x y)
   (vector-ref *cells* (+ (* x *width*) y)))

(define (for-each-board f)
   (let loop ((i 0))
      (if (< i *width*)
	  (begin
	     (let loop ((j 0))
		(if (< j *height*)
		    (begin
		       (f i j)
		       (loop (+ j 1)))))
	     (loop (+ i 1))))))

(define (for-each-cell f)
   (let ((nb-cells (* *width* *height*)))
      (let loop ((i 0))
	 (if (< i nb-cells)
	  (begin
	     (f (vector-ref *cells* i))
	     (loop (+ i 1)))))))

(define (for-each-neighbor f c)
   (define (valid-cell x y)
      (and (>= x 0)
	   (< x *width*)
	   (>= y 0)
	   (< y *height*)))

   (let loop ((i (- c.mine.x 1)))
      (if (<= i (+ c.mine.x 1))
	  (begin
	     (let loop ((j (- c.mine.y 1)))
		(if (<= j (+ c.mine.y 1))
		    (begin
		       (if (and (valid-cell i j)
				(not (and (= i c.mine.x)
					  (= j c.mine.y))))
			   (f (cell i j)))
		       (loop (+ j 1)))))
	     (loop (+ i 1))))))

(define (game-over! won?)
   (set! *game-over* #t)
   (if (not won?)
       (for-each-cell (lambda (cell)
			 (reveal! cell))))
   ;; avoids the "image dragging" in most cases.
   (alert (if won? "You won" "You lost")))

(define (nb-neighbor-mines cell)
   (let ((nb-neighbors 0))
      (for-each-neighbor (lambda (cell)
			    (if cell.mine.mined?
				(set! nb-neighbors (+ nb-neighbors 1))))
			 cell)
      nb-neighbors))
   
(define (reveal! cell)
   (let ((mine cell.mine))
      ;(debugException)
      (cond
	 (mine.revealed?
	  'done)
	 ((and (not mine.mined?) mine.bomb-marked?)
	  (set! cell.src *bomb-misflagged-img*))
	 ((and mine.mined? (not mine.bomb-marked?))
	  (set! cell.src *bomb-revealed-img*))
	 ((and mine.mined? mine.bomb-marked?)
	  'done)
	 (else
	  (set! cell.src (vector-ref *revealed-imgs* mine.nb-neighbors))))
      (set! mine.revealed? #t)))

(define (cell-click! cell)
   (let ((mine cell.mine))
      (cond
	 (mine.bomb-marked?
	  'done)
	 (mine.mined?
	  (set! cell.src *bomb-death-img*)
	  (set! mine.revealed? #t)
	  (game-over! #f))
	 (mine.revealed?
	  'done)
	 (else
	  (reveal! cell)
	  (if (= mine.nb-neighbors 0)
	      (for-each-neighbor cell-click! cell))
	  (set! *missing-cells* (- *missing-cells* 1))
	  (if (= *missing-cells* 0)
	      (game-over! #t))))))

(define (mark-bomb! cell)
   (let ((mine cell.mine))
      (cond
	 (mine.revealed?
	  'do-nothing)
	 (mine.bomb-marked?
	  (set! mine.bomb-marked? #f)
	  (set! *nb-marked-bombs* (- *nb-marked-bombs* 1))
	  (set! cell.src *blank-img*))
	 (else
	  (set! mine.bomb-marked? #t)
	  (set! *nb-marked-bombs* (+ *nb-marked-bombs* 1))
	  (set! cell.src *bomb-flagged-img*)))
      (remaining-bombs-update!)))

(define (cell-mouse-down event cell)
   (stop-event-propagation event)
   (if (and (not *game-over*)
	    (or (eq? event.which 1) (eq? event.button 0))) ; left button
       (if event.shiftKey
	   (mark-bomb! cell)
	   (cell-click! cell))))

(define (cell-create! x y)
   (define (Mine)
      (set! this.x x)
      (set! this.y y)
      (set! this.mined? #f)
      (set! this.bomb-marked? #f)
      (set! this.revealed? #f))
   
   (let ((cell (document.createElement "img")))
      (set! cell.src *blank-img*)
      (set! cell.mine (js-new Mine))
      (add-event-listener! cell
			   "click"
			   (lambda (event)
			      (cell-mouse-down event cell)))
      cell))

(define *cells* (let ((v (make-vector (* *width* *height*)))
		      (c 0))
		   (for-each-board (lambda (x y)
				      (vector-set! v c
						   (cell-create! x y))
				      (set! c (+ c 1))))
		   v))

(define (board-init!)
   (let ((board-div (document.getElementById "board"))
	 (line-div #f))
      (node-style-set! board-div "padding" 0)
      (node-style-set! board-div "margin" 0)
      (node-style-set! board-div "border" 0)
      (set! board-div.style.visibility "hidden")
      (for-each-cell (lambda (cell)
			(node-style-set! cell "padding" 0)
			(node-style-set! cell "margin" 0)
			(node-style-set! cell "border" 0)
			(if (= cell.mine.y 0)
			    (begin
			       (set! line-div (document.createElement "div"))
			       (node-style-set! line-div "padding" 0)
			       (node-style-set! line-div "margin" 0)
			       (node-style-set! line-div "border" 0)
			       (node-style-set! line-div "height" "15px")
			       (board-div.appendChild line-div)))
			(line-div.appendChild cell)))
      (set! board-div.style.visibility "visible")))

(define (game-start!)
   (set! *missing-cells* (- (* *width* *height*) *nb-mines*))
   (set! *game-over* #f)
   (set! *nb-marked-bombs* 0)
   (for-each-cell (lambda (c)
		     (let ((mine c.mine))
			(set! mine.mined? #f)
			(set! mine.bomb-marked? #f)
			(set! mine.revealed? #f)
			(set! c.src *blank-img*))))
   (let loop ((i 0))
      (if (< i *nb-mines*)
	  (let* ((x (Math.floor (* (Math.random) *width*)))
		 (y (Math.floor (* (Math.random) *height*)))
		 (c (cell x y)))
	     (if (not c.mine.mined?)
		 (begin
		    (set! c.mine.mined? #t)
		    (loop (+ i 1)))
		 (loop i)))))
   (for-each-cell (lambda (c)
			(set! c.mine.nb-neighbors (nb-neighbor-mines c))))
   (remaining-bombs-update!))

   
