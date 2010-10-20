;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/hop_rss.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan  8 08:36:26 2007                          */
;*    Last change :  Wed Oct 20 09:35:33 2010 (serrano)                */
;*    Copyright   :  2007-10 Manuel Serrano                            */
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
	    __hop_xml-types
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
(define-method (xml-write obj::xml-rss p backend)
   (with-access::xml-rss obj (header tag attributes body)
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
			  (symbol->string (hop-charset)))))))
      (instantiate::xml-rss
	 (tag 'rss)
	 (header hd)
	 (attributes (cons* (cons 'version version)
			    (cons 'xmlns:dc xmlns:dc)
			    attributes))
	 (body body))))

(define-xml-markup <RSS:CHANNEL> :tag channel)
(define-xml-markup <RSS:TITLE> :tag title)
(define-xml-markup <RSS:LINK> :tag link)
(define-xml-markup <RSS:DESCRIPTION> :tag description)
(define-xml-markup <RSS:LANGUAGE> :tag language)
(define-xml-markup <RSS:COPYRIGHT> :tag copyright)
(define-xml-markup <RSS:MANAGING-EDITOR> :tag managingEditor)
(define-xml-markup <RSS:WEB-MASTER> :tag webMaster)
(define-xml-markup <RSS:PUB-DATE> :tag pubDate)
(define-xml-markup <RSS:LAST-BUILD-DATE> :tag lastBuildDate)
(define-xml-markup <RSS:CATEGORY> :tag category)
(define-xml-markup <RSS:GENERATOR> :tag generator)
(define-xml-markup <RSS:DOCS> :tag docs)
(define-xml-markup <RSS:CLOUD> :tag cloud)
(define-xml-markup <RSS:TTL> :tag ttl)
(define-xml-markup <RSS:IMAGE> :tag image)
(define-xml-markup <RSS:URL> :tag url)
(define-xml-markup <RSS:RATING> :tag rating)
(define-xml-markup <RSS:TEXT-INPUT> :tag textInput)
(define-xml-markup <RSS:SKIP-HOURS> :tag skipHours)
(define-xml-markup <RSS:SKIP-DAYS> :tag skipDays)
(define-xml-markup <RSS:ITEM> :tag item)
(define-xml-markup <RSS:AUTHOR> :tag author)
(define-xml-markup <RSS:COMMENTS> :tag comments)
(define-xml-markup <RSS:ENCLOSURE> :tag enclosure)
(define-xml-markup <RSS:GUID> :tag guid)
(define-xml-markup <RSS:SOURCE> :tag source)
