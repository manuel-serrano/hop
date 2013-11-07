;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/nodes.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-12                                           */
;*    Last change :  Tue Nov  5 15:37:44 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme2js AST definition                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module nodes
   
   (import verbose
	   export-desc)
   (export
      
      ;; Labels used for breaks and continues
      (final-class Label
	 (id::symbol read-only))
      
      ;; Variables
      (final-class Var
	 (id::symbol read-only)
	 
	 ;; one out of local, exported, imported or this
	 (kind::symbol read-only) 
	 ;; export-desc is used for exported and imported vars.
	 (export-desc::Export-Desc (default (class-nil Export-Desc)) read-only)
	 
	 (constant?::bool (default #f))
	 (value (default #f))
	 (uses::bint (default 0))
	 (captured?::bool (default #f))
	 (escapes?::bool (default #f))
	 (mutated-outside-local?::bool (default #f))
	 
	 (needs-boxing?::bool (default #f))
	 (needs-frame?::bool (default #f))
	 (needs-uniquization?::bool (default #f))
	 (needs-update?::bool (default #f))
	 
	 ;; not essential, but handy.
	 (indirect?::bool (default #f)))

      ;; Nodes
      (class Node
	 (location (default #f))
	 (dstposition (default #f)))
      
      (final-class Const::Node
	 value)
      
      (final-class Ref::Node
	 ;; either symbol or qualified id of form (symbol module)
	 id 
	 (var::Var (default (class-nil Var))))

      (class Scope::Node
	 ;; list of Vars
	 (scope-vars::pair-nil (default '())) 
	 (call/cc?::bool (default #f)))

      ;; basically Modules and Lambdas
      (class Execution-Unit::Scope 
	 ;; this-vars are always instantiated, but might not be used during
	 ;; symbol-resolution
	 (this-var::Var (default (instantiate::Var (id 'this) (kind 'this)))
	    read-only)
	 body::Node
	 (free-vars::pair-nil (default '()))
	 ;; declared vars need to be declared using "var".
	 ;; most often declared-vars includes scope-vars.
	 ;; However, scope-vars contains parameters of functions, but they
	 ;; are not inside 'declared-vars'.
	 (declared-vars::pair-nil (default '())))
      
      ;; all scope-vars are exported.
      (final-class Module::Execution-Unit
	 ;; list of Vars
	 (runtime-vars::pair-nil  (default '()))
	 ;; list of Vars
	 (imported-vars::pair-nil (default '())))
      
      ;; if the fun was vaarg, then the flag vaarg? has to be set, and the
      ;; vaarg must be the last element of the formals.
      (final-class Lambda::Execution-Unit
	 ;; list of Refs
	 formals::pair-nil           
	 vaarg?::bool
	 (arity::int read-only)
	 (closure?::bool (default #f))
	 (nested-closures?::bool (default #f))
	 (size::bint (default 0))
	 (call/cc-finally-scopes::pair-nil (default '()))
	 (call/cc-nb-while-counters::bint (default 0))
	 (call/cc-nb-indices::bint (default 0))
	 ;; a-list (index rev-hoisted ...)
	 (call/cc-hoisted::pair-nil (default '())) 
	 (call/cc-contained-scopes::pair-nil (default '()))
	 (contains-trampoline-call?::bool (default #f))
	 (isloop?::bool (default #f)))
      
      (final-class If::Node
	 test::Node
	 then::Node
	 else::Node)
      
      (final-class Case::Node
	 key::Node
	 clauses::pair-nil)
      
      (final-class Clause::Node
	 ;; list of Const. default clause has no consts
	 consts::pair-nil            
	 expr::Node
	 default-clause?::bool)
      
      (final-class Set!::Node
	 lvalue::Ref
	 val::Node)
      
      (final-class Let::Scope
	 ;; list of Set!s (at least initially)
	 bindings::pair-nil          
	 body::Node
	 ;; either 'let or 'letrec
	 kind::symbol)
      
      (final-class Begin::Node
	 exprs::pair-nil
	 (call/cc?::bool (default #f))
	 (call/cc-ranges::pair-nil (default '())))
      
      (final-class Call::Node
	 operator::Node
	 operands::pair-nil
	 (call/cc?::bool (default #f))
	 ;; #f -> tail call
	 (call/cc-index (default #f)) 
	 (trampoline?::bool (default #f)))
      
      ;; optimization-nodes
      (final-class Frame-alloc::Node
	 storage-var::Var
	 vars::pair-nil)
      
      (final-class Frame-push::Node
	 frame-alloc::Frame-alloc
	 body::Node)

      (final-class Return::Node
	 val::Node)
      
      (final-class Labeled::Node
	 body::Node
	 label::Label)
      
      ;; break must never reference a loop.
      ;; if this is needed, then the loop must be wrapped into a 'labeled'.
      (final-class Break::Node
	 val::Node
	 label::Label)
      
      (final-class Continue::Node
	 label::Label)
      
      (final-class Pragma::Node
	 str::bstring
	 args::pair-nil)
      
      (final-class Tail-rec::Scope
	 ;; inits are Set!s (no particular order)
	 inits::pair-nil
	 body::Node
	 label::Label)
      
      (final-class Tail-rec-Call::Node
	 ;; updates here are not assignments
	 ;; they must be in the same order, as the scope-vars in the Tail-rec.
	 updates::pair-nil
	 label::Label)
      
      (final-class While::Scope
	 init::Node
	 test::Node
	 body::Node
	 label::Label
	 (call/cc-finally-scopes::pair-nil (default '()))
	 (call/cc-counter-nb::bint (default -1)))
      
      (final-class Call/cc-Resume::Node
	 indices::pair-nil)
      
      (Ref-of-new-Var::Ref id::symbol #!key (location #f))
      (var-reference v::Var #!key (location #f))
      (var-assig v::Var val::Node #!key (location #f))
      
      (default-label::Label)))

;*---------------------------------------------------------------------*/
;*    var-reference ...                                                */
;*---------------------------------------------------------------------*/
(define (var-reference v::Var #!key (location #f))
   (instantiate::Ref
      (location (if (isa? location Node)
		    (with-access::Node location (location) location)
		    location))
      (id (with-access::Var v (id) id))
      (var v)))

;*---------------------------------------------------------------------*/
;*    var-assig ...                                                    */
;*---------------------------------------------------------------------*/
(define (var-assig v::Var val::Node #!key (location #f))
   (let* ((var-ref (var-reference v :location location))
	  (location (cond
		       ((isa? location Node)
			(with-access::Node location (location) location))
		       (location
			location)
		       (else
			(-> val location)))))
      (instantiate::Set!
	 (location (if (isa? location Node)
		       (with-access::Node location (location) location)
		       location))
	 (lvalue var-ref)
	 (val val))))

;*---------------------------------------------------------------------*/
;*    Ref-of-new-Var ...                                               */
;*---------------------------------------------------------------------*/
(define (Ref-of-new-Var id #!key (location #f))
   (let ((var (instantiate::Var
		 (id id)
		 (kind 'local))))
      (instantiate::Ref
	 (location (if (isa? location Node)
		       (with-access::Node location (location) location)
		       location))
	 (id id)
	 (var var))))

;*---------------------------------------------------------------------*/
;*    default-label ...                                                */
;*---------------------------------------------------------------------*/
(define (default-label) *default-label*)

(define *default-label* (instantiate::Label (id 'default)))
