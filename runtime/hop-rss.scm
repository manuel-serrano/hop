;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/hop-rss.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan  8 08:36:26 2007                          */
;*    Last change :  Mon Jan  8 18:18:56 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HOP RSS bindings                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_hop-rss

   (include "xml.sch")

   (import  __hop_param
	    __hop_types
	    __hop_xml
	    __hop_misc
	    __hop_js-lib
	    __hop_service)

   (static  (class xml-rss::xml-markup
	       (header::bstring read-only)))
   
   (export  (<RSS> . ::obj)
	    
	    (<RSS:CHANNEL> . ::obj)
	    (<RSS:TITLE> . ::obj)
	    (<RSS:LINK> . ::obj)
	    (<RSS:DESCRIPTION> . ::obj)
	    (<RSS:LANGUAGE> . ::obj)
	    (<RSS:COPYRIGHT> . ::obj)
	    (<RSS:MANAGING-EDITOR> . ::obj)
	    (<RSS:WEB-MASTER> . ::obj)
	    (<RSS:PUB-DATE> . ::obj)
	    (<RSS:LAST-BUILD-DATE> . ::obj)
	    (<RSS:CATEGORY> . ::obj)
	    (<RSS:GENERATOR> . ::obj)
	    (<RSS:DOCS> . ::obj)
	    (<RSS:CLOUD> . ::obj)
	    (<RSS:TTL> . ::obj)
	    (<RSS:IMAGE> . ::obj)
	    (<RSS:URL> . ::obj)
	    (<RSS:RATING> . ::obj)
	    (<RSS:TEXT-INPUT> . ::obj)
	    (<RSS:SKIP-HOURS> . ::obj)
	    (<RSS:SKIP-DAYS> . ::obj)

	    (<RSS:ITEM> . ::obj)
	    (<RSS:AUTHOR> . ::obj)
	    (<RSS:COMMENTS> . ::obj)
	    (<RSS:ENCLOSURE> . ::obj)	
	    (<RSS:GUID> . ::obj)
	    (<RSS:SOURCE> . ::obj)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-rss ...                                          */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-rss p encoding backend)
   (with-access::xml-rss obj (header markup attributes body)
      (display header p)
      (newline p)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    Standards RSS elements                                           */
;*---------------------------------------------------------------------*/
(define-xml-compound <RSS> ((id #unspecified string)
			    (version "2.0")
			    (xmlns:dc "http://purl.org/dc/elements/1.1/")
			    (header #f)
			    (attributes)
			    body)
   (let ((hd (or header
		 (format "<?xml version=\"1.0\" encoding=\"~a\"?>"
			 (string-downcase
			  (symbol->string (hop-char-encoding)))))))
      (instantiate::xml-rss
	 (markup 'rss)
	 (header hd)
	 (attributes (cons* (cons 'version version)
			    (cons 'xmlns:dc xmlns:dc)
			    attributes))
	 (body body))))

(define-xml-markup <RSS:CHANNEL> :markup channel)
(define-xml-markup <RSS:TITLE> :markup title)
(define-xml-markup <RSS:LINK> :markup link)
(define-xml-markup <RSS:DESCRIPTION> :markup description)
(define-xml-markup <RSS:LANGUAGE> :markup language)
(define-xml-markup <RSS:COPYRIGHT> :markup copyright)
(define-xml-markup <RSS:MANAGING-EDITOR> :markup managingEditor)
(define-xml-markup <RSS:WEB-MASTER> :markup webMaster)
(define-xml-markup <RSS:PUB-DATE> :markup pubDate)
(define-xml-markup <RSS:LAST-BUILD-DATE> :markup lastBuildDate)
(define-xml-markup <RSS:CATEGORY> :markup category)
(define-xml-markup <RSS:GENERATOR> :markup generator)
(define-xml-markup <RSS:DOCS> :markup docs)
(define-xml-markup <RSS:CLOUD> :markup cloud)
(define-xml-markup <RSS:TTL> :markup ttl)
(define-xml-markup <RSS:IMAGE> :markup image)
(define-xml-markup <RSS:URL> :markup url)
(define-xml-markup <RSS:RATING> :markup rating)
(define-xml-markup <RSS:TEXT-INPUT> :markup textInput)
(define-xml-markup <RSS:SKIP-HOURS> :markup skipHours)
(define-xml-markup <RSS:SKIP-DAYS> :markup skipDays)
(define-xml-markup <RSS:ITEM> :markup item)
(define-xml-markup <RSS:AUTHOR> :markup author)
(define-xml-markup <RSS:COMMENTS> :markup comments)
(define-xml-markup <RSS:ENCLOSURE> :markup enclosure)
(define-xml-markup <RSS:GUID> :markup guid)
(define-xml-markup <RSS:SOURCE> :markup source)
