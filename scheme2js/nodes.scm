(module nodes
   (import verbose)
   (export
    ;; Labels used for breaks and continues
    (final-class Label
       (id::symbol read-only))

    ;; Variables
    (class Var
       (id::symbol read-only)

       (already-defined?::bool (default #f)) ;; TODO: remove (side)
       (constant?::bool (default #f))
       (value (default #f))
       (uses::bint (default 0))
       (captured?::bool (default #f))
       (escapes?::bool (default #f))

       ;; see scope.scm for the following fields.
       (call/cc-when-alive?::bool (default #f))
       (modified-after-call/cc?::bool (default #f))
       (needs-boxing?::bool (default #f))
       (needs-uniquization?::bool (default #f))
       (needs-frame?::bool (default #f))
       (needs-update?::bool (default #f))
       (indirect?::bool (default #f))

       ;; see propagation for the following field
       (escaping-mutated?::bool (default #f))
       (current (default #f))

       ;; see out for the following field
       (js-id::bstring (default "")))
    (final-class Local::Var)
    (class JS-Var::Var)
    (final-class Imported-Var::JS-Var
       (exported-as-const?::bool read-only)
       (runtime?::bool read-only)
       (peephole       (default #f) read-only)
       (higher?::bool  (default #f) read-only)
       (higher-params  (default #f) read-only) 
       (return-type    (default #f) read-only))
    (final-class Exported-Var::JS-Var
       exported-as-const?::bool
       (return-type (default #f) read-only))
    (final-class This-Var::JS-Var)

    ;; ===========================  Nodes ==========================
    (class Node
       (location (default #f)))
    (final-class Const::Node
       value)
    (final-class Ref::Node
       id::symbol
       (var::Var (default (Var-nil))))
    (class Scope::Node
       (scope-vars::pair-nil (default '()))) ;; list of Vars
    (class Execution-Unit::Scope ;; basically Modules and Lambdas
       ;; this-vars are always instantiated, but might not be used during
       ;; symbol-resolution
       (this-var::This-Var (default (instantiate::This-Var
					  (id 'this)
					  (js-id "this")))
			   read-only)
       body::Node
       
       (free-vars::pair-nil (default '()))

       (declared-vars::pair-nil (default '()))
       (contained-scopes::pair-nil (default '())))
    
    (final-class Module::Execution-Unit
       ;; all scope-vars are exported.

       (runtime-vars::pair-nil  (default '())) ;; list of Vars
       (imported-vars::pair-nil (default '())) ;; list of Vars

       )
    (final-class Lambda::Execution-Unit
       ;; if the fun was vaarg, then the flag vaarg? has to be set, and the
       ;; vaarg must be the last element of the formals.
       formals::pair-nil           ;; list of Refs
       vaarg?::bool

       (closure?::bool (default #f))
       (nested-closures?::bool (default #f))

       (size::bint (default 0))
       )
    (final-class If::Node
       test::Node
       then::Node
       else::Node)
    (final-class Case::Node
       key::Node
       clauses::pair-nil)          ;; list of Clause
    (final-class Clause::Node
       consts::pair-nil            ;; list of Const. default clause has no consts
       expr::Node
       default-clause?::bool)
    (final-class Set!::Node
       lvalue::Ref
       val::Node)
    (final-class Let::Scope
       bindings::pair-nil
       body::Node
       kind::symbol) ;; either 'let or 'letrec
    (final-class Begin::Node
       exprs::pair-nil)
    (class Call::Node
       operator::Node
       operands::pair-nil)
    (final-class SCall::Call) ;; so we can widen it.

   ;; optimization-nodes

    (final-class Frame-alloc::Node
       storage-var::Var
       vars::pair-nil)
    (final-class Frame-push::Node
       storage-vars::pair-nil
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
       str::bstring)
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

       (contained-scopes::pair-nil (default '()))
       )
    (final-class Call/cc-Call::Call
       (visible-scopes::pair-nil (default '())))
    (final-class Call/cc-Resume::Node
       ;(set! this.indices/vars (list (cons index #f))))
       indices/vars::pair-nil)
    (final-class Call/cc-Counter-Update::Node
       index::bint)

    (Ref-of-new-Var::Ref id::symbol)
    (var-reference v::Var)
    (var-assig v::Var val::Node)

    (default-label::Label)
    ))

(define (var-reference v::Var)
   (instantiate::Ref
      (id (Var-id v))
      (var v)))

(define (var-assig v::Var val::Node)
   (let ((var-ref (var-reference v)))
      (instantiate::Set!
	 (lvalue var-ref)
	 (val val))))

(define (Ref-of-new-Var id)
   (let* ((var (instantiate::Local
		  (id id))))
      (instantiate::Ref
	 (id id)
	 (var var))))

(define *default-label* (instantiate::Label (id 'default)))
(define (default-label) *default-label*)
