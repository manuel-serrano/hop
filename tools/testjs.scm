;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/tools/testjs.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 27 17:13:14 2013                          */
;*    Last change :  Fri Nov  4 15:55:26 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Testing JavaScript programs                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module testjs
   (static (class &test-error::&error
	      path))
   (main main)
   (option (bigloo-compiler-debug-set! 1)))

;*---------------------------------------------------------------------*/
;*    skip-errors ...                                                  */
;*---------------------------------------------------------------------*/
(define skip-errors 0)

;*---------------------------------------------------------------------*/
;*    hopc ...                                                         */
;*---------------------------------------------------------------------*/
(define hopc "hopc")
(define hopcflags "")

;*---------------------------------------------------------------------*/
;*    directories ...                                                  */
;*---------------------------------------------------------------------*/
(define directories '())

(define /tmp "/tmp/TESTJS")

;*---------------------------------------------------------------------*/
;*    flags ...                                                        */
;*---------------------------------------------------------------------*/
(define flags '())

;*---------------------------------------------------------------------*/
;*    recursive ...                                                    */
;*---------------------------------------------------------------------*/
(define recursive #t)

;*---------------------------------------------------------------------*/
;*    score                                                            */
;*---------------------------------------------------------------------*/
(define global-count 0)

(define global-ok 0)
(define global-fail 0)
(define global-skip 0)
(define local-ok 0)
(define local-fail 0)
(define local-skip 0)

;*---------------------------------------------------------------------*/
;*    blacklist                                                        */
;*---------------------------------------------------------------------*/
(define blacklist '())
(define blacklist-file #t)

;*---------------------------------------------------------------------*/
;*    include-path ...                                                 */
;*---------------------------------------------------------------------*/
(define include-path
   '("console/harness"
     "website/harness"
     "external/contributions/Google/sputniktests/lib"))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   
   (bigloo-debug-set! 1)

   (make-directories /tmp)
   
   (args-parse (cdr args)
      (("--hopc=?path" (help "Hopc path"))
       (set! hopc path))
      (("--flags=?flags" (help "Hopc flags"))
       (set! hopcflags flags))
      (("-v?num" (help "Debug and verbosity"))
       (if (string=? num "")
	   (bigloo-debug-set! (+fx (bigloo-debug) 1))
	   (bigloo-debug-set! (string->integer num))))
      (("-s?num" (help "Set the number of error to accept"))
       (if (string=? num "")
	   (set! skip-errors -1)
	   (set! skip-errors (string->integer num))))
      (("-r" (help "Recursive traversal"))
       (set! recursive #t))
      (("-f" (help "No recursive traversal"))
       (set! recursive #f))
      (("--no-blacklist-file" (help "Disable blacklisting"))
       (set! blacklist-file #f))
      ((("-b" "--blacklist") ?dir (help "Don't scan directory"))
       (set! blacklist (cons dir blacklist)))
      ((("-I" "--include-path") ?dir (help "Add include directory"))
       (set! include-path (cons dir include-path)))
      (("--help" (help "This message"))
       (args-parse-usage #f)
       (exit 0))
      (else
       (if (string=? else "--")
	   (set! flags (cdr rest))
	   (set! directories (cons else directories)))))

   (when (and blacklist-file (file-exists? "BLACKLIST"))
      (set! blacklist
	 (append blacklist
	    (call-with-input-file "BLACKLIST" read))))

   (for-each (lambda (file)
		(set! baselen (string-length (dirname file)))
		(if (directory? file)
		    (test-directory file (make-include-path file))
		    (test-path file (make-include-path file))))
      (reverse! directories))

   (when (> (+ global-ok global-fail) 0)
      (print "\n\nglobal score="
	 (round (* (/ global-ok (+ global-ok global-fail)) 100))
	 "% (ok=" global-ok ", fail=" global-fail ", skip=" global-skip
	 ")")))

;*---------------------------------------------------------------------*/
;*    make-include-path ...                                            */
;*---------------------------------------------------------------------*/
(define (make-include-path file)
   (let ((dir (car include-path)))
      (let loop ((base file))
	 (let ((path (make-file-name base dir)))
	    (if (directory? path)
		(map (lambda (dir) (make-file-name base dir)) include-path)
		(let ((dirname (dirname base)))
		   (if (string=? dirname base)
		       '()
		       (loop dirname))))))))

;*---------------------------------------------------------------------*/
;*    baselen ...                                                      */
;*---------------------------------------------------------------------*/
(define baselen #f)

;*---------------------------------------------------------------------*/
;*    blacklist? ...                                                   */
;*---------------------------------------------------------------------*/
(define (blacklist? name blacklist)
   (any (lambda (re)
	   (pregexp-match re name))
      blacklist))

;*---------------------------------------------------------------------*/
;*    test-directory ...                                               */
;*---------------------------------------------------------------------*/
(define (test-directory dir include-path)
   (let ((old-ok local-ok)
	 (old-fail local-fail)
	 (old-skip local-skip))
      (unwind-protect
	 (let ((b (basename dir)))
	    (unless (blacklist? b blacklist)
	       (with-trace 1 (substring dir (+fx baselen 1))
		  (set! local-ok 0)
		  (set! local-fail 0)
		  (set! local-skip 0)
		  (for-each (lambda (path)
			       (cond
				  ((directory? path)
				   (when recursive (test-directory path include-path)))
				  ((not (blacklist? (basename path) blacklist))
				   (test-path path include-path))
				  (else
				   (set! local-skip (+fx 1 local-skip)))))
		     (sort (lambda (s1 s2)
			      (<fx (string-natural-compare3 s1 s2) 0))
			(directory->path-list dir))))))
	 (begin
	    (set! global-ok (+fx global-ok local-ok))
	    (set! global-fail (+fx global-fail local-fail))
	    (set! global-skip (+fx global-skip local-skip))
	    (when (or (> local-ok 0) (> local-fail 0) (> local-skip 0))
	       (let ((ok (+ global-ok old-ok))
		     (fail (+ global-fail old-fail))
		     (skip (+ global-skip old-skip)))
		  (when (> (+ local-ok local-fail) 0)
		     (trace-item "local score="
			(round (* (/ local-ok (+ local-ok local-fail)) 100))
			"% (ok=" local-ok ", fail=" local-fail ", skip=" local-skip
			") global score="
			(round (* (/ ok (+ ok fail)) 100))
			"% (ok=" ok ", fail=" fail ", skip=" skip
			")"))))
	    (set! local-skip old-skip)
	    (set! local-ok old-ok)
	    (set! local-fail old-fail)))))

;*---------------------------------------------------------------------*/
;*    test-path ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-path path include-path)
   (unless (blacklist? (basename path) blacklist)
      (set! global-count (+fx 1 global-count))
      (bind-exit (skip)
	 (with-handler
	    (lambda (e)
	       ;; mark the failure
	       (set! local-fail (+fx 1 local-fail))
	       (with-trace 1 (format "TEST ~a FAILED"
				(+ global-ok global-fail local-ok local-fail))
		  (if (isa? e &test-error)
		      (with-access::&test-error e (proc msg obj path)
			 (trace-item proc)
			 (trace-item path)
			 (trace-item msg)
			 (fprint (current-error-port) obj)
			 (if (=fx skip-errors 0)
			     (exit 1)
			     (begin
				(set! skip-errors (-fx skip-errors 1))
				(skip #t))))
		      (raise e))))
	    (with-trace 2 (format "~a (~a)" (basename path) global-count)
	       (let ((tmp (make-file-path /tmp (basename path))))
		  ;; generate the actual source file)
		  (let ((isnegative (generate-test-file! path tmp include-path)))
		     ;; compile it
		     (for-each (lambda (f)
				  (let ((p (make-file-name /tmp f)))
				     (when (file-exists? p)
					(delete-file p))))
			'("testjs-err.comp"
			  "testjs.comp"
			  "testjs-err.exec"
			  "testjs.exec"))
		     (let ((ccmd (compile-test tmp isnegative)))
			;; execute it
			(when (string? ccmd)
			   (execute-test tmp ccmd))))
		  ;; mark success
		  (set! local-ok (+fx 1 local-ok))))))))

;*---------------------------------------------------------------------*/
;*    negative-table ...                                               */
;*---------------------------------------------------------------------*/
(define kmp-negative-table (kmp-table "@negative"))
(define kmp-include-table (kmp-table "$INCLUDE"))
(define kmp-getprecision-table (kmp-table "getPrecision"))
(define kmp-isequal-table (kmp-table "isEqual"))
   
;*---------------------------------------------------------------------*/
;*    generate-test-file! ...                                          */
;*---------------------------------------------------------------------*/
(define (generate-test-file! path tmp include-path)
   (let ((body (call-with-input-file path read-string))
	 (isnegative #f))
      (call-with-output-file tmp
	 (lambda (p)
	    (when (or (>fx (kmp-string kmp-getprecision-table body 0) 0)
		      (>fx (kmp-string kmp-isequal-table body 0) 0))
	       (display "function getPrecision(num) {
   log2num = Math.log(Math.abs(num))/Math.LN2;
   pernum = Math.ceil(log2num);
   return(2 * Math.pow(2, -52 + pernum));
}

var prec;
function isEqual(num1, num2) {
   if ((num1 === Infinity)&&(num2 === Infinity)) {
      return(true);
   }
   if ((num1 === -Infinity)&&(num2 === -Infinity)) {
      return(true);
   }
   prec = getPrecision(Math.min(Math.abs(num1), Math.abs(num2)));  
   return(Math.abs(num1 - num2) <= prec);
}\n" p))
	    (if (>fx (kmp-string kmp-negative-table body 0) 0)
		(begin
		   (set! isnegative #t)
		   (display "try {\n" p)
		   (display "(function() {\n" p)
		   (display body p)
		   (display "ERROR( 'Should have raised an error' );\n" p)
		   (display "})();\n" p)
		   (display "} catch( e ) { quit( 0 ); }" p))
		(display body p))
	    (display "\n\nfunction ERROR( s ) { console.log( \"Error: \", s ); quit( 1 ) }\n" p)
	    (display "function $ERROR( s ) { console.log( \"Error: \", s ); quit( 1 ) }\n" p)
	    (display "function $FAIL( s ) { console.log( \"Fail: \", s ); quit( 1 ) }\n" p)
	    (display "function $PRINT( s ) { console.log( s ); }\n" p)
	    (display "function Test262Error() { return false; }\n" p)
	    (display "function fnGlobalObject() { return this; }\n" p)
	    (display "function runTestCase( proc ) { if( !proc() ) $ERROR( 'Test failed..' ); }\n" p)
	    (display "function fnExists( proc ) { return typeof( proc ) === 'function'; }" p)
	    (display "function compareArray( a1, a2 ) {
if( a1.length != a2.length ) return false;
for( var i = 0; i < a1.length; i++ ) {
  if( a1[ i ] != a2[ i ] ) return false;
}
return true; };\n" p)
	    (display "function arrayContains( a2, a1 ) {  for( var p in a1) {if (a2.indexOf( a1[ p ] ) < 0) return false;} return true;}\n" p)
	    (display "function dataPropertyAttributesAreCorrect( obj, prop, val, writable, enumerable, configurable ) {
  var o = Object.getOwnPropertyDescriptor( obj, prop );
  var r = (((obj[prop] === val) || (isNaN(val) && isNaN(obj[prop]))) && o.writable === writable && o.enumerable === enumerable && o.configurable === configurable);
  if( !r ) {
    console.log( \"prop=\", obj[prop], \"/\", val );
    console.log( \"write=\", o.writable, \"/\", writable );
    console.log( \"enum=\", o.enumerable, \"/\", enumerable );
    console.log( \"conf=\", o.configurable, \"/\", configurable );
    return false;
  } else {
    return true;
  }}\n" p)
	    (display "function accessorPropertyAttributesAreCorrect( obj, prop, get, set, _, enumerable, configurable ) {
  var o = Object.getOwnPropertyDescriptor( obj, prop );
  var r = (get === o.get && o.set === set && o.enumerable === enumerable && o.configurable === configurable);
  if( !r ) {
    console.log( \"get=\", o.get, \"/\", get );
    console.log( \"set=\", o.set, \"/\", set );
    console.log( \"enum=\", o.enumerable, \"/\", enumerable );
    console.log( \"conf=\", o.configurable, \"/\", configurable );
    return false;
  } else {
  return true;
  }}; \n" p)
	    (display "function quit( code ) { process.exit( code ); }\n" p)))
      (when (>fx (kmp-string kmp-include-table body 0) 0)
	 ;; the file contains an include directive, it has to be postprocessed
	 (let ((tmpfile (string-append tmp ".tmp")))
	    (rename-file tmp tmpfile)
	    (unwind-protect
	       (call-with-output-file tmp
		  (lambda (op)
		     (expand-file tmpfile op include-path)))
	       (delete-file tmpfile))))
      isnegative))

;*---------------------------------------------------------------------*/
;*    expand-file ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-file file op include-path)
   (call-with-input-file file
      (lambda (ip)
	 (let loop ()
	    (let ((line (read-line-newline ip)))
	       (unless (eof-object? line)
		  (if (string-prefix? "$INCLUDE(" line)
		      (include-file line op include-path)
		      (display line op))
		  (loop)))))))
   
;*---------------------------------------------------------------------*/
;*    include-file ...                                                 */
;*---------------------------------------------------------------------*/
(define (include-file line op include-path)
   (let ((m (pregexp-match "^[$]INCLUDE[(][ ]*\"([^\"]+)\"[ ]*[)];?$" line)))
      (when m
	 (let ((file (cadr m)))
	    (expand-file (find-include file include-path) op include-path)))))

;*---------------------------------------------------------------------*/
;*    find-include ...                                                 */
;*---------------------------------------------------------------------*/
(define (find-include file include-path)
   (any (lambda (dir)
	   (let ((path (make-file-name dir file)))
	      (when (file-exists? path)
		 path)))
      include-path))

;*---------------------------------------------------------------------*/
;*    compile-test ...                                                 */
;*---------------------------------------------------------------------*/
(define (compile-test tmp isnegative)
   (let ((cmd (format "~a ~a ~a -o ~a ~a" hopc hopcflags tmp (prefix tmp)
		 (apply string-append flags))))
      (with-trace 3 cmd
	 (let ((res (system (format "~a 2> ~a/testjs-err.comp > ~a/testjs.comp" cmd /tmp /tmp))))
	    (cond
	       ((=fx res 0)
		cmd)
	       (isnegative
		#f)
	       (else
		(raise
		   (instantiate::&test-error
		      (proc "compile")
		      (msg cmd)
		      (path tmp)
		      (obj (system->string (string-append cmd " 2>&1")))))))))))

;*---------------------------------------------------------------------*/
;*    execute-test ...                                                 */
;*---------------------------------------------------------------------*/
(define (execute-test tmp ccmd)
   (let ((cmd (prefix tmp)))
      (unless (=fx (system (format "~a 2> ~a/testjs-err.exec > ~a/testjs.exec" cmd /tmp /tmp)) 0)
	 (raise
	    (instantiate::&test-error
	       (proc "exec")
	       (msg ccmd)
	       (path tmp)
	       (obj (system->string (string-append cmd " 2>&1"))))))))
      


