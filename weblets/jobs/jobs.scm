;*=====================================================================*/
;*    serrano/prgm/project/hop/weblets/jobs/jobs.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 14 06:14:00 2005                          */
;*    Last change :  Fri Jan 20 14:58:02 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP jobs control                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_jobs)

;*---------------------------------------------------------------------*/
;*    jobs weblet ...                                                  */
;*---------------------------------------------------------------------*/
(define-weblet (jobs)
   (define (<CODE> expr)
      (<PRE> :class "code"
	     (with-output-to-string
		(lambda ()
		   (pp expr)))))
   (define (<JOB> j rec id)
      (<DIV>
       :class "job"
       (<B> :class "cancel"
	    :onclick { if( true || confirm( "Cancel job" ) ) {
			    hop( $(service( jn )
				     (job-cancel! (job-find jn))
                                     (rec))( $(job-name j) ),
				 hop_replace_id( $id ) ) } }
	    "Cancel!")
       
       (<TABLE>
	(<TR> (<TH> "Expression:") (<TD> (<CODE> (job-expression j))))
	(<TR> (<TH> "Restore:") (<TD> (<CODE> (job-restore j))))
	(<TR> (<TH> "Date:") (<TD> (seconds->date (job-date j))))
	(<TR> (<TH> "Repeat:") (<TD> (job-repeat j)))
	(<TR> (<TH> "Interval:") (<TD> (job-interval j))))))
   (<HTML>
    (<HEAD>
     (<HOP-HEAD> :css "hop-notepad.css"
		 :jscript "hop-notepad.js"
		 :css "weblets/jobs/jobs.hss"))
    (<BODY>
     (<NOTEPAD>
      :id "notepad"
      :inline #t
      (<NPHEAD> "Hop Jobs Control")
      (<NPTAB> (<NPTABHEAD> "Waiting")
	       (<DIV> :id "waiting"
		      (let loop ()
			 (let ((j (jobs-queue)))
			    (if (pair? j)
				(<TABLE>
				 (map (lambda (j)
					 (<TR> (<TD> (<JOB> j loop "waiting"))))
				      j))
				"No waiting job")))))
      (<NPTAB> (<NPTABHEAD> "Running")
	       (<DIV> :id "running"
		      (let loop ()
			 (let ((j (jobs-run)))
			    (if (pair? j)
				(<TABLE>
				 (<TR> (<TD>))
				 (map (lambda (j)
					 (<TR> (<TD> (<JOB> j loop "running"))))
				      j))
				"No running job")))))))))
