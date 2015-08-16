;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/hopreplay/main.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:30:13 2004                          */
;*    Last change :  Sun Jun 17 08:57:26 2012 (serrano)                */
;*    Copyright   :  2004-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOPREPLAY entry point                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hoprp

   (library scheme2js hopscheme hop web)

   (import  hoprp_parseargs
	    hoprp_param
	    hoprp_replay
	    hoprp_log)

   (main    main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   ;; set the Hop cond-expand identification
   (for-each register-eval-srfi! (hop-srfis))
   ;; set the library load path
   (bigloo-library-path-set! (hop-library-path))
   ;; preload the hop library
   (eval `(library-load 'hop))
   ;; parse the command line
   (let ((logs (append-map parse-log (parse-args args))))
      (hop-verb 1 "Starting hopreplay (v" (hop-version) "):\n")
      ;; setup the hop readers
      (bigloo-load-reader-set! hop-read)
      ;; start the hop main loop
      (replay logs)))
