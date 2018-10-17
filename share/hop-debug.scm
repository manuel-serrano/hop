;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/share/hop-debug.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 29 14:46:23 2013                          */
;*    Last change :  Tue Aug  7 09:58:03 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop runtime debugging support                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop-debug
   
   (include
      "values.sch"
      "../runtime/base64_vlq.sch"
      "../runtime/sourcemap.sch")
   
   (import
      __hop-exception)
   
   (export
      (hop-source-mapping-url file url))
   
   (scheme2js-pragma
      (hop-source-mapping-url (JS "hop_source_mapping_url"))))

;*---------------------------------------------------------------------*/
;*    source-file-table ...                                            */
;*---------------------------------------------------------------------*/
(define source-file-table
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    source-map-table ...                                             */
;*---------------------------------------------------------------------*/
(define source-map-table
   (make-hashtable))

;*---------------------------------------------------------------------*/
;*    hop-source-mapping-url ...                                       */
;*---------------------------------------------------------------------*/
(define (hop-source-mapping-url file url)
   (hashtable-put! source-file-table file url))

;*---------------------------------------------------------------------*/
;*    get-source-map ...                                               */
;*---------------------------------------------------------------------*/
(define (get-source-map url)
   (or (hashtable-get source-map-table url)
       (with-hop ($(service :name "public/server-debug/source-map" (file)) url)
	  :sync #t
	  :anim #f
	  (lambda (h)
	     (hashtable-put! source-map-table url h)
	     h)
	  (lambda (f)
	     #f))))

;*---------------------------------------------------------------------*/
;*    hop-debug-source-map-file ...                                    */
;*---------------------------------------------------------------------*/
(define (hop-debug-source-map-file file line col)
   (let* ((i (string-index file #\?))
          (name (if i (substring file 0 i) file))
          (url (hashtable-get source-file-table name)))
      (if url
	  (let ((smap (get-source-map url)))
	     (if smap
		 (hop-debug-source-map-file/smap smap.mappings smap.sources
		    file line col)
		 (values #f #f #f)))
	  (values #f #f #f))))

;*---------------------------------------------------------------------*/
;*    hop-source-map-register! ...                                     */
;*---------------------------------------------------------------------*/
(hop-source-map-register! hop-debug-source-map-file)
