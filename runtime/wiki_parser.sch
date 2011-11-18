;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Thu Nov 10 17:26:58 CET 2011 
;; (bigloo wiki_parser.scm -classgen)
;; ==========================================================

;; The directives
(directives
   (cond-expand ((and bigloo-class-sans (not bigloo-class-generate))

;; state
(static
  (inline state?::bool ::obj)
  (inline make-state::state markup2276::symbol syntax2277::procedure expr2278::pair-nil attr2279::pair-nil value2280::obj)
  (inline state-value::obj ::state)
  (inline state-value-set! ::state ::obj)
  (inline state-attr::pair-nil ::state)
  (inline state-attr-set! ::state ::pair-nil)
  (inline state-expr::pair-nil ::state)
  (inline state-expr-set! ::state ::pair-nil)
  (inline state-syntax::procedure ::state)
  (inline state-syntax-set! ::state ::procedure)
  (inline state-markup::symbol ::state)
  (inline state-markup-set! ::state ::symbol)
)
;; expr
(static
  (inline expr?::bool ::obj)
  (inline make-expr::expr markup2269::symbol syntax2270::procedure expr2271::pair-nil attr2272::pair-nil value2273::obj)
  (inline expr-value::obj ::expr)
  (inline expr-value-set! ::expr ::obj)
  (inline expr-attr::pair-nil ::expr)
  (inline expr-attr-set! ::expr ::pair-nil)
  (inline expr-expr::pair-nil ::expr)
  (inline expr-expr-set! ::expr ::pair-nil)
  (inline expr-syntax::procedure ::expr)
  (inline expr-syntax-set! ::expr ::procedure)
  (inline expr-markup::symbol ::expr)
  (inline expr-markup-set! ::expr ::symbol)
)
;; block
(static
  (inline block?::bool ::obj)
  (inline make-block::block markup2261::symbol syntax2262::procedure expr2263::pair-nil attr2264::pair-nil value2265::obj is-subblock2266::obj)
  (inline block-is-subblock::obj ::block)
  (inline block-value::obj ::block)
  (inline block-value-set! ::block ::obj)
  (inline block-attr::pair-nil ::block)
  (inline block-attr-set! ::block ::pair-nil)
  (inline block-expr::pair-nil ::block)
  (inline block-expr-set! ::block ::pair-nil)
  (inline block-syntax::procedure ::block)
  (inline block-syntax-set! ::block ::procedure)
  (inline block-markup::symbol ::block)
  (inline block-markup-set! ::block ::symbol)
)
;; plugin
(static
  (inline plugin?::bool ::obj)
  (inline make-plugin::plugin markup2254::symbol syntax2255::procedure expr2256::pair-nil attr2257::pair-nil value2258::obj)
  (inline plugin-value::obj ::plugin)
  (inline plugin-value-set! ::plugin ::obj)
  (inline plugin-attr::pair-nil ::plugin)
  (inline plugin-attr-set! ::plugin ::pair-nil)
  (inline plugin-expr::pair-nil ::plugin)
  (inline plugin-expr-set! ::plugin ::pair-nil)
  (inline plugin-syntax::procedure ::plugin)
  (inline plugin-syntax-set! ::plugin ::procedure)
  (inline plugin-markup::symbol ::plugin)
  (inline plugin-markup-set! ::plugin ::symbol)
)
)))

;; The definitions
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate));; state
(define-inline (state?::bool obj::obj) ((@ isa? __object) obj (@ state __hop_wiki-parser)))
(define-inline (make-state::state markup2276::symbol syntax2277::procedure expr2278::pair-nil attr2279::pair-nil value2280::obj) (instantiate::state (markup markup2276) (syntax syntax2277) (expr expr2278) (attr attr2279) (value value2280)))
(define-inline (state-value::obj o::state) (with-access::state o (value) value))
(define-inline (state-value-set! o::state v::obj) (with-access::state o (value) (set! value v)))
(define-inline (state-attr::pair-nil o::state) (with-access::state o (attr) attr))
(define-inline (state-attr-set! o::state v::pair-nil) (with-access::state o (attr) (set! attr v)))
(define-inline (state-expr::pair-nil o::state) (with-access::state o (expr) expr))
(define-inline (state-expr-set! o::state v::pair-nil) (with-access::state o (expr) (set! expr v)))
(define-inline (state-syntax::procedure o::state) (with-access::state o (syntax) syntax))
(define-inline (state-syntax-set! o::state v::procedure) (with-access::state o (syntax) (set! syntax v)))
(define-inline (state-markup::symbol o::state) (with-access::state o (markup) markup))
(define-inline (state-markup-set! o::state v::symbol) (with-access::state o (markup) (set! markup v)))

;; expr
(define-inline (expr?::bool obj::obj) ((@ isa? __object) obj (@ expr __hop_wiki-parser)))
(define-inline (make-expr::expr markup2269::symbol syntax2270::procedure expr2271::pair-nil attr2272::pair-nil value2273::obj) (instantiate::expr (markup markup2269) (syntax syntax2270) (expr expr2271) (attr attr2272) (value value2273)))
(define-inline (expr-value::obj o::expr) (with-access::expr o (value) value))
(define-inline (expr-value-set! o::expr v::obj) (with-access::expr o (value) (set! value v)))
(define-inline (expr-attr::pair-nil o::expr) (with-access::expr o (attr) attr))
(define-inline (expr-attr-set! o::expr v::pair-nil) (with-access::expr o (attr) (set! attr v)))
(define-inline (expr-expr::pair-nil o::expr) (with-access::expr o (expr) expr))
(define-inline (expr-expr-set! o::expr v::pair-nil) (with-access::expr o (expr) (set! expr v)))
(define-inline (expr-syntax::procedure o::expr) (with-access::expr o (syntax) syntax))
(define-inline (expr-syntax-set! o::expr v::procedure) (with-access::expr o (syntax) (set! syntax v)))
(define-inline (expr-markup::symbol o::expr) (with-access::expr o (markup) markup))
(define-inline (expr-markup-set! o::expr v::symbol) (with-access::expr o (markup) (set! markup v)))

;; block
(define-inline (block?::bool obj::obj) ((@ isa? __object) obj (@ block __hop_wiki-parser)))
(define-inline (make-block::block markup2261::symbol syntax2262::procedure expr2263::pair-nil attr2264::pair-nil value2265::obj is-subblock2266::obj) (instantiate::block (markup markup2261) (syntax syntax2262) (expr expr2263) (attr attr2264) (value value2265) (is-subblock is-subblock2266)))
(define-inline (block-is-subblock::obj o::block) (with-access::block o (is-subblock) is-subblock))
(define-inline (block-is-subblock-set! o::block v::obj) (with-access::block o (is-subblock) (set! is-subblock v)))
(define-inline (block-value::obj o::block) (with-access::block o (value) value))
(define-inline (block-value-set! o::block v::obj) (with-access::block o (value) (set! value v)))
(define-inline (block-attr::pair-nil o::block) (with-access::block o (attr) attr))
(define-inline (block-attr-set! o::block v::pair-nil) (with-access::block o (attr) (set! attr v)))
(define-inline (block-expr::pair-nil o::block) (with-access::block o (expr) expr))
(define-inline (block-expr-set! o::block v::pair-nil) (with-access::block o (expr) (set! expr v)))
(define-inline (block-syntax::procedure o::block) (with-access::block o (syntax) syntax))
(define-inline (block-syntax-set! o::block v::procedure) (with-access::block o (syntax) (set! syntax v)))
(define-inline (block-markup::symbol o::block) (with-access::block o (markup) markup))
(define-inline (block-markup-set! o::block v::symbol) (with-access::block o (markup) (set! markup v)))

;; plugin
(define-inline (plugin?::bool obj::obj) ((@ isa? __object) obj (@ plugin __hop_wiki-parser)))
(define-inline (make-plugin::plugin markup2254::symbol syntax2255::procedure expr2256::pair-nil attr2257::pair-nil value2258::obj) (instantiate::plugin (markup markup2254) (syntax syntax2255) (expr expr2256) (attr attr2257) (value value2258)))
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
