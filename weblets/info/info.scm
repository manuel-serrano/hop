;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/weblets/info/info.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 25 18:43:56 2010                          */
;*    Last change :  Thu Oct 25 17:00:56 2012 (serrano)                */
;*    Copyright   :  2010-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Client side of the INFO weblet                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module weblet_info-client
   
   $(import weblet_info)

   (import weblet_info-maze)
   
   (export (make-script-default-kont id kont)
	   (make-network-default-kont id kont)
	   (run-client-benchmark base-val node-val node-score kont)
	   (run-dom-benchmark base-val node-val node-score kont)
	   (run-server-benchmark base-val node-val node-score kont)
	   (run-network-keepalive-benchmark base-val img node-val node-score kont)
	   (run-network-close-benchmark base-val img node-val node-score kont)
	   (run-network-file-benchmark base-val img node-val node-score kont)
	   (run-network-withhop-benchmark base-val img node-val node-score kont)))

;*---------------------------------------------------------------------*/
;*    bench-min-time ...                                               */
;*---------------------------------------------------------------------*/
(define (bench-min-time) 10000)

;*---------------------------------------------------------------------*/
;*    bench-script-md5sum ...                                          */
;*---------------------------------------------------------------------*/
(define (bench-script-md5sum)
   "1a02a1b00c483ab99e58a0b80bcc7520")

;*---------------------------------------------------------------------*/
;*    scores ...                                                       */
;*---------------------------------------------------------------------*/
(define script-scores (list #f #f 0))
(define network-scores (list #f #f #f #f))

;*---------------------------------------------------------------------*/
;*    make-script-kont ...                                             */
;*---------------------------------------------------------------------*/
(define (make-script-kont n kont)
   (lambda (s)
      (set-car! (list-tail script-scores n) s)
      (if (every number? script-scores)
	  (kont (apply + script-scores))
	  (kont #f))))

;*---------------------------------------------------------------------*/
;*    make-script-default-kont ...                                     */
;*---------------------------------------------------------------------*/
(define (make-script-default-kont id kont)
   (lambda (s)
      (when (number? s)
	 (innerHTML-set! id s))
      (when (procedure? kont)
	 (kont s))))
   
;*---------------------------------------------------------------------*/
;*    run-benchmark ...                                                */
;*    -------------------------------------------------------------    */
;*    Execute proc as many times as needed to reach ms milliseconds    */
;*---------------------------------------------------------------------*/
(define (run-benchmark proc callback)
   ;; first run to estimate the number of executions
   (proc (lambda (t v)
	    (let ((n (round (/fx (bench-min-time) t))))
	       (let loop ((i 0)
			  (t t))
		  ;; show the progress bar
		  (when (callback (round (* 100 (/ i n))) (/ t n) v)
		     ;; next run
		     (proc (lambda (nt nv)
			      (loop (+ i 1) (+ t nt))))))))))

;*---------------------------------------------------------------------*/
;*    run-server-benchmark ...                                         */
;*---------------------------------------------------------------------*/
(define (run-server-benchmark base-val node-val node-score kont)
   (innerHTML-set! "perfs-benchmark-name" "server script...")
   (gauge-value-set! "perfs-gauge" 0)
   (run-benchmark
    (lambda (next)
       (with-hop ($info/perfs/script/server)
	  (lambda (vh)
	     (next (car vh) (cdr vh)))))
    (let ((k (make-script-kont 0 kont)))
       (make-callback base-val node-val node-score (bench-script-md5sum) k))))

;*---------------------------------------------------------------------*/
;*    run-client-benchmark ...                                         */
;*---------------------------------------------------------------------*/
(define (run-client-benchmark base-val node-val node-score kont)
   (innerHTML-set! "perfs-benchmark-name" "client script...")
   (gauge-value-set! "perfs-gauge" 0)
   (run-benchmark
    (lambda (next)
       (multiple-value-bind (res rtime stime utime)
	  (time (lambda () (maze 20)))
	  (after 10
	     (lambda ()
		(next rtime (md5sum (apply string-append res)))))))
    (let ((k (make-script-kont 1 kont)))
       (make-callback base-val node-val node-score (bench-script-md5sum) k))))

;*---------------------------------------------------------------------*/
;*    run-dom-benchmark ...                                            */
;*---------------------------------------------------------------------*/
(define (run-dom-benchmark base-val node-val node-score kont)
   (innerHTML-set! "perfs-benchmark-name" "client dom...")
   (gauge-value-set! "perfs-gauge" 0)
   (run-benchmark
    (lambda (next)
       (multiple-value-bind (res rtime stime utime)
	  (time (lambda () (dom-benchmark 20)))
	  (after 10
	     (lambda ()
		(next rtime 20)))))
    (let ((k (make-script-kont 1 kont)))
       (make-callback base-val node-val node-score 20 k))))

;*---------------------------------------------------------------------*/
;*    svc-key ...                                                      */
;*---------------------------------------------------------------------*/
(define svc-key 0)

;*---------------------------------------------------------------------*/
;*    get-svc-key ...                                                  */
;*---------------------------------------------------------------------*/
(define (get-svc-key)
   (set! svc-key (+ 1 svc-key))
   svc-key)

;*---------------------------------------------------------------------*/
;*    run-network-benchmark ...                                        */
;*---------------------------------------------------------------------*/
(define (run-network-benchmark svc base-val id node-val node-score k)
   (let ((img (dom-get-element-by-id id))
	 (proc (make-callback base-val node-val node-score #t k))
	 (t0 (current-microseconds)))
      (set! img.onload (lambda ()
			  (let* ((t (- (current-microseconds) t0))
				 (n (round (/ (bench-min-time) t)))
				 (i 0))
			     (set! img.onload
				   (lambda ()
				      (let ((t (- (current-microseconds) t0)))
					 (set! i (+ i 1))
					 (when (proc (round (* 100 (/ i n)))
						     (/ t n)
						     #t)
					    (set! img.src
						  (svc :key (get-svc-key)))))))
			     (set! img.src (svc :key (get-svc-key))))))
      (set! img.src (svc :key (get-svc-key)))))

;*---------------------------------------------------------------------*/
;*    make-network-kont ...                                            */
;*---------------------------------------------------------------------*/
(define (make-network-kont n kont)
   (lambda (s)
      (set-car! (list-tail network-scores n) s)
      (if (every number? network-scores)
	  (kont (apply + network-scores))
	  (kont #f))))

;*---------------------------------------------------------------------*/
;*    make-network-default-kont ...                                    */
;*---------------------------------------------------------------------*/
(define (make-network-default-kont id kont)
   (lambda (s)
      (when (number? s)
	 (innerHTML-set! id s))
      (when (procedure? kont)
	 (kont s))))
   
;*---------------------------------------------------------------------*/
;*    run-network-keepalive-benchmark ...                              */
;*---------------------------------------------------------------------*/
(define (run-network-keepalive-benchmark base-val id node-val node-score kont)
   (innerHTML-set! "perfs-benchmark-name" "network keep-alive...")
   (let ((k (make-network-kont 0 kont)))
      (run-network-benchmark $info/perfs/network/keepalive base-val id node-val node-score k)))

;*---------------------------------------------------------------------*/
;*    run-network-close-benchmark ...                                  */
;*---------------------------------------------------------------------*/
(define (run-network-close-benchmark base-val id node-val node-score kont)
   (innerHTML-set! "perfs-benchmark-name" "network close...")
   (let ((k (make-network-kont 1 kont)))
      (run-network-benchmark $info/perfs/network/close base-val id node-val node-score k)))

;*---------------------------------------------------------------------*/
;*    run-network-file-benchmark ...                                   */
;*---------------------------------------------------------------------*/
(define (run-network-file-benchmark base-val id node-val node-score kont)
   (innerHTML-set! "perfs-benchmark-name" "network file...")
   (let ((k (make-network-kont 2 kont)))
      (run-network-benchmark $info/perfs/network/file base-val id node-val node-score k)))

;*---------------------------------------------------------------------*/
;*    run-network-withhop-benchmark ...                                */
;*---------------------------------------------------------------------*/
(define (run-network-withhop-benchmark base-val id node-val node-score kont)
   (innerHTML-set! "perfs-benchmark-name" "network with-hop...")
   (let* ((k (make-network-kont 3 kont))
	  (proc (make-callback base-val node-val node-score 0 k))
	 (t0 (current-microseconds))
	 (i 0))
      (with-hop ($info/perfs/network/withhop)
	 (lambda (vh)
	    (let* ((t (- (current-microseconds) t0))
		   (n (round (/ (bench-min-time) t))))
	       (let loop ((i 0))
		  (when (proc (round (* 100 (/ i n))) (/ t n) 0)
		     (with-hop ($info/perfs/network/withhop :key (get-svc-key))
			(lambda (h)
			   (loop (+ i 1)))))))))))

;*---------------------------------------------------------------------*/
;*    make-callback ...                                                */
;*---------------------------------------------------------------------*/
(define (make-callback base-val node-val node-score res kont)
   (lambda (n v r)
      (cond
	 ((not (equal? r res))
	  (innerHTML-set! node-val "-1")
	  (innerHTML-set! node-score "-1")
	  (dom-add-class! node-val "perfs-error")
	  (dom-add-class! node-score "perfs-error")
	  #f)
	 ((>= n 100)
	  (let ((score (round (* 100 (/ base-val v)))))
	     (innerHTML-set! node-val (/ (round (* 100 v)) 100))
	     (innerHTML-set! node-score score)
	     (gauge-value-set! "perfs-gauge" 100)
	     (if (> v base-val)
		 (begin
		    (dom-add-class! node-score "perfs-slow")
		    (dom-add-class! node-val "perfs-slow"))
		 (begin
		    (dom-add-class! node-score "perfs-fast")
		    (dom-add-class! node-val "perfs-fast")))
	     (kont score)
	     #f))
	 (else
	  (gauge-value-set! "perfs-gauge" n)
	  #t))))

;*---------------------------------------------------------------------*/
;*    dom-benchmark ...                                                */
;*---------------------------------------------------------------------*/
(define (dom-benchmark n)
   
   (define (node-benchmark)
      ;; deep clone all the child nodes
      (for-each (lambda (n)
		   (dom-clone-node n #t))
		(dom-child-nodes (dom-get-element-by-id "perfs-div")))
      ;; inverse all the child nodes
      (let* ((p (dom-get-element-by-id "perfs-div"))
	     (l (dom-child-nodes p)))
	 (for-each (lambda (n)
		      (dom-remove-child! p n)
		      (dom-append-child! p n))
		   l))
      ;; reset the parent node
      (innerHTML-set! "perfs-div" ""))
   
   (let loop ((i 0))
      (when (<fx i n)
	 ;; create a html tree from a string
	 (innerHTML-set! "perfs-div" (<SPAN> (get-html-string)))
	 (node-benchmark)
	 ;; create a html tree from an expression
	 (innerHTML-set! "perfs-div" "")
	 (dom-append-child! (dom-get-element-by-id "perfs-div") (get-html-tree))
	 ;; reset and loop
	 (innerHTML-set! "perfs-div" "")
	 ;; loop again
	 (loop (+ i 1))))
   n)

;*---------------------------------------------------------------------*/
;*    get-html-tree ...                                                */
;*---------------------------------------------------------------------*/
(define get-html-tree
   (let ((tree (<FILECHOOSER> $(hop-share-directory))))
      (lambda ()
	 tree)))

;*---------------------------------------------------------------------*/
;*    get-html-string ...                                              */
;*---------------------------------------------------------------------*/
(define get-html-string
   (let* ((t (get-html-tree))
	  (s t.innerHTML))
      (lambda ()
	 s)))

