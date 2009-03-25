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

       (kind::symbol read-only) ;; one out of local, exported, imported or this
       ;; export-desc is used for exported and imported vars.
       (export-desc::Export-Desc (default (Export-Desc-nil)) read-only)

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

       (indirect?::bool (default #f)) ;; not essential, but handy.

       (already-defined?::bool (default #f)) ;; TODO: remove (side)
       )

    ;; ===========================  Nodes ==========================
    (class Node
       (location (default #f)))
    (final-class Const::Node
       value)
    (final-class Ref::Node
       id ;; either symbol or qualified id of form (symbol module)
       (var::Var (default (Var-nil))))
    (class Scope::Node
       (scope-vars::pair-nil (default '())) ;; list of Vars

       (call/cc?::bool (default #f)))
    (class Execution-Unit::Scope ;; basically Modules and Lambdas
       ;; this-vars are always instantiated, but might not be used during
       ;; symbol-resolution
       (this-var::Var (default (instantiate::Var
					  (id 'this)
					  (kind 'this)))
			   read-only)
       body::Node
       
       (free-vars::pair-nil (default '()))

       (declared-vars::pair-nil (default '())))
    
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

       (call/cc-finally-scopes::pair-nil (default '()))
       (call/cc-nb-while-counters::bint (default 0))
       (call/cc-nb-indices::bint (default 0))
       (call/cc-hoisted::pair-nil (default '())) ;; a-list (index rev-hoisted ...)
       (call/cc-contained-scopes::pair-nil (default '()))

       (contains-trampoline-call?::bool (default #f))
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
       bindings::pair-nil          ;; list of Set!s (at least initially)
       body::Node
       kind::symbol) ;; either 'let or 'letrec
    (final-class Begin::Node
       exprs::pair-nil

       (call/cc?::bool (default #f))
       (call/cc-ranges::pair-nil (default '())))
    (final-class Call::Node
       operator::Node
       operands::pair-nil

       (call/cc?::bool (default #f))
       (call/cc-index (default #f)) ;; #f -> tail call

       (trampoline?::bool (default #f)))

   ;; optimization-nodes

    (final-class Frame-alloc::Node
       storage-var::Var
       vars::pair-nil)
    (final-class Frame-push::Node
       frame-allocs::pair-nil
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

       (call/cc-finally-scopes::pair-nil (default '()))
       (call/cc-counter-nb::bint (default -1))
       )
    (final-class Call/cc-Resume::Node
       indices::pair-nil)

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
   (let* ((var (instantiate::Var
		  (id id)
		  (kind 'local))))
      (instantiate::Ref
	 (id id)
	 (var var))))

(define *default-label* (instantiate::Label (id 'default)))
(define (default-label) *default-label*)
