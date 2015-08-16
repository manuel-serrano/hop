;; ==========================================================
;; Class accessors
;; Bigloo (3.8d)
;; Inria -- Sophia Antipolis     Sat Oct 13 08:56:28 CEST 2012 
;; (bigloo wiki_parser.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; WikiState
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-WikiState::WikiState markup1144::symbol syntax1145::procedure expr1146::pair-nil attr1147::pair-nil value1148::obj)
    (inline WikiState?::bool ::obj)
    (WikiState-nil::WikiState)
    (inline WikiState-value::obj ::WikiState)
    (inline WikiState-value-set! ::WikiState ::obj)
    (inline WikiState-attr::pair-nil ::WikiState)
    (inline WikiState-attr-set! ::WikiState ::pair-nil)
    (inline WikiState-expr::pair-nil ::WikiState)
    (inline WikiState-expr-set! ::WikiState ::pair-nil)
    (inline WikiState-syntax::procedure ::WikiState)
    (inline WikiState-syntax-set! ::WikiState ::procedure)
    (inline WikiState-markup::symbol ::WikiState)
    (inline WikiState-markup-set! ::WikiState ::symbol))))

;; WikiExpr
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-WikiExpr::WikiExpr markup1138::symbol syntax1139::procedure expr1140::pair-nil attr1141::pair-nil value1142::obj)
    (inline WikiExpr?::bool ::obj)
    (WikiExpr-nil::WikiExpr)
    (inline WikiExpr-value::obj ::WikiExpr)
    (inline WikiExpr-value-set! ::WikiExpr ::obj)
    (inline WikiExpr-attr::pair-nil ::WikiExpr)
    (inline WikiExpr-attr-set! ::WikiExpr ::pair-nil)
    (inline WikiExpr-expr::pair-nil ::WikiExpr)
    (inline WikiExpr-expr-set! ::WikiExpr ::pair-nil)
    (inline WikiExpr-syntax::procedure ::WikiExpr)
    (inline WikiExpr-syntax-set! ::WikiExpr ::procedure)
    (inline WikiExpr-markup::symbol ::WikiExpr)
    (inline WikiExpr-markup-set! ::WikiExpr ::symbol))))

;; WikiBlock
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-WikiBlock::WikiBlock markup1131::symbol syntax1132::procedure expr1133::pair-nil attr1134::pair-nil value1135::obj is-subblock1136::obj)
    (inline WikiBlock?::bool ::obj)
    (WikiBlock-nil::WikiBlock)
    (inline WikiBlock-is-subblock::obj ::WikiBlock)
    (inline WikiBlock-value::obj ::WikiBlock)
    (inline WikiBlock-value-set! ::WikiBlock ::obj)
    (inline WikiBlock-attr::pair-nil ::WikiBlock)
    (inline WikiBlock-attr-set! ::WikiBlock ::pair-nil)
    (inline WikiBlock-expr::pair-nil ::WikiBlock)
    (inline WikiBlock-expr-set! ::WikiBlock ::pair-nil)
    (inline WikiBlock-syntax::procedure ::WikiBlock)
    (inline WikiBlock-syntax-set! ::WikiBlock ::procedure)
    (inline WikiBlock-markup::symbol ::WikiBlock)
    (inline WikiBlock-markup-set! ::WikiBlock ::symbol))))

;; plugin
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-plugin::plugin markup1125::symbol syntax1126::procedure expr1127::pair-nil attr1128::pair-nil value1129::obj)
    (inline plugin?::bool ::obj)
    (plugin-nil::plugin)
    (inline plugin-value::obj ::plugin)
    (inline plugin-value-set! ::plugin ::obj)
    (inline plugin-attr::pair-nil ::plugin)
    (inline plugin-attr-set! ::plugin ::pair-nil)
    (inline plugin-expr::pair-nil ::plugin)
    (inline plugin-expr-set! ::plugin ::pair-nil)
    (inline plugin-syntax::procedure ::plugin)
    (inline plugin-syntax-set! ::plugin ::procedure)
    (inline plugin-markup::symbol ::plugin)
    (inline plugin-markup-set! ::plugin ::symbol)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; WikiState
(define-inline (make-WikiState::WikiState markup1144::symbol syntax1145::procedure expr1146::pair-nil attr1147::pair-nil value1148::obj) (instantiate::WikiState (markup markup1144) (syntax syntax1145) (expr expr1146) (attr attr1147) (value value1148)))
(define-inline (WikiState?::bool obj::obj) ((@ isa? __object) obj (@ WikiState __hop_wiki-parser)))
(define (WikiState-nil::WikiState) (class-nil (@ WikiState __hop_wiki-parser)))
(define-inline (WikiState-value::obj o::WikiState) (with-access::WikiState o (value) value))
(define-inline (WikiState-value-set! o::WikiState v::obj) (with-access::WikiState o (value) (set! value v)))
(define-inline (WikiState-attr::pair-nil o::WikiState) (with-access::WikiState o (attr) attr))
(define-inline (WikiState-attr-set! o::WikiState v::pair-nil) (with-access::WikiState o (attr) (set! attr v)))
(define-inline (WikiState-expr::pair-nil o::WikiState) (with-access::WikiState o (expr) expr))
(define-inline (WikiState-expr-set! o::WikiState v::pair-nil) (with-access::WikiState o (expr) (set! expr v)))
(define-inline (WikiState-syntax::procedure o::WikiState) (with-access::WikiState o (syntax) syntax))
(define-inline (WikiState-syntax-set! o::WikiState v::procedure) (with-access::WikiState o (syntax) (set! syntax v)))
(define-inline (WikiState-markup::symbol o::WikiState) (with-access::WikiState o (markup) markup))
(define-inline (WikiState-markup-set! o::WikiState v::symbol) (with-access::WikiState o (markup) (set! markup v)))

;; WikiExpr
(define-inline (make-WikiExpr::WikiExpr markup1138::symbol syntax1139::procedure expr1140::pair-nil attr1141::pair-nil value1142::obj) (instantiate::WikiExpr (markup markup1138) (syntax syntax1139) (expr expr1140) (attr attr1141) (value value1142)))
(define-inline (WikiExpr?::bool obj::obj) ((@ isa? __object) obj (@ WikiExpr __hop_wiki-parser)))
(define (WikiExpr-nil::WikiExpr) (class-nil (@ WikiExpr __hop_wiki-parser)))
(define-inline (WikiExpr-value::obj o::WikiExpr) (with-access::WikiExpr o (value) value))
(define-inline (WikiExpr-value-set! o::WikiExpr v::obj) (with-access::WikiExpr o (value) (set! value v)))
(define-inline (WikiExpr-attr::pair-nil o::WikiExpr) (with-access::WikiExpr o (attr) attr))
(define-inline (WikiExpr-attr-set! o::WikiExpr v::pair-nil) (with-access::WikiExpr o (attr) (set! attr v)))
(define-inline (WikiExpr-expr::pair-nil o::WikiExpr) (with-access::WikiExpr o (expr) expr))
(define-inline (WikiExpr-expr-set! o::WikiExpr v::pair-nil) (with-access::WikiExpr o (expr) (set! expr v)))
(define-inline (WikiExpr-syntax::procedure o::WikiExpr) (with-access::WikiExpr o (syntax) syntax))
(define-inline (WikiExpr-syntax-set! o::WikiExpr v::procedure) (with-access::WikiExpr o (syntax) (set! syntax v)))
(define-inline (WikiExpr-markup::symbol o::WikiExpr) (with-access::WikiExpr o (markup) markup))
(define-inline (WikiExpr-markup-set! o::WikiExpr v::symbol) (with-access::WikiExpr o (markup) (set! markup v)))

;; WikiBlock
(define-inline (make-WikiBlock::WikiBlock markup1131::symbol syntax1132::procedure expr1133::pair-nil attr1134::pair-nil value1135::obj is-subblock1136::obj) (instantiate::WikiBlock (markup markup1131) (syntax syntax1132) (expr expr1133) (attr attr1134) (value value1135) (is-subblock is-subblock1136)))
(define-inline (WikiBlock?::bool obj::obj) ((@ isa? __object) obj (@ WikiBlock __hop_wiki-parser)))
(define (WikiBlock-nil::WikiBlock) (class-nil (@ WikiBlock __hop_wiki-parser)))
(define-inline (WikiBlock-is-subblock::obj o::WikiBlock) (with-access::WikiBlock o (is-subblock) is-subblock))
(define-inline (WikiBlock-is-subblock-set! o::WikiBlock v::obj) (with-access::WikiBlock o (is-subblock) (set! is-subblock v)))
(define-inline (WikiBlock-value::obj o::WikiBlock) (with-access::WikiBlock o (value) value))
(define-inline (WikiBlock-value-set! o::WikiBlock v::obj) (with-access::WikiBlock o (value) (set! value v)))
(define-inline (WikiBlock-attr::pair-nil o::WikiBlock) (with-access::WikiBlock o (attr) attr))
(define-inline (WikiBlock-attr-set! o::WikiBlock v::pair-nil) (with-access::WikiBlock o (attr) (set! attr v)))
(define-inline (WikiBlock-expr::pair-nil o::WikiBlock) (with-access::WikiBlock o (expr) expr))
(define-inline (WikiBlock-expr-set! o::WikiBlock v::pair-nil) (with-access::WikiBlock o (expr) (set! expr v)))
(define-inline (WikiBlock-syntax::procedure o::WikiBlock) (with-access::WikiBlock o (syntax) syntax))
(define-inline (WikiBlock-syntax-set! o::WikiBlock v::procedure) (with-access::WikiBlock o (syntax) (set! syntax v)))
(define-inline (WikiBlock-markup::symbol o::WikiBlock) (with-access::WikiBlock o (markup) markup))
(define-inline (WikiBlock-markup-set! o::WikiBlock v::symbol) (with-access::WikiBlock o (markup) (set! markup v)))

;; plugin
(define-inline (make-plugin::plugin markup1125::symbol syntax1126::procedure expr1127::pair-nil attr1128::pair-nil value1129::obj) (instantiate::plugin (markup markup1125) (syntax syntax1126) (expr expr1127) (attr attr1128) (value value1129)))
(define-inline (plugin?::bool obj::obj) ((@ isa? __object) obj (@ plugin __hop_wiki-parser)))
(define (plugin-nil::plugin) (class-nil (@ plugin __hop_wiki-parser)))
(define-inline (plugin-value::obj o::plugin) (with-access::plugin o (value) value))
(define-inline (plugin-value-set! o::plugin v::obj) (with-access::plugin o (value) (set! value v)))
(define-inline (plugin-attr::pair-nil o::plugin) (with-access::plugin o (attr) attr))
(define-inline (plugin-attr-set! o::plugin v::pair-nil) (with-access::plugin o (attr) (set! attr v)))
(define-inline (plugin-expr::pair-nil o::plugin) (with-access::plugin o (expr) expr))
(define-inline (plugin-expr-set! o::plugin v::pair-nil) (with-access::plugin o (expr) (set! expr v)))
(define-inline (plugin-syntax::procedure o::plugin) (with-access::plugin o (syntax) syntax))
(define-inline (plugin-syntax-set! o::plugin v::procedure) (with-access::plugin o (syntax) (set! syntax v)))
(define-inline (plugin-markup::symbol o::plugin) (with-access::plugin o (markup) markup))
(define-inline (plugin-markup-set! o::plugin v::symbol) (with-access::plugin o (markup) (set! markup v)))
))
