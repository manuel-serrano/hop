/*=====================================================================*/
/*    Author      :  Florian Loitsch                                   */
/*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
/*    -------------------------------------------------------------    */
/*    This file is part of Scheme2Js.                                  */
/*                                                                     */
/*   Scheme2Js is distributed in the hope that it will be useful,      */
/*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
/*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
/*   LICENSE file for more details.                                    */
/*=====================================================================*/

/*
 * Some functions that make call/cc inside JavaScript easier:
 * - CallCcException have a .pushCont method. This method accepts a function which
 *   represents the continuation. (see for example for-each).
 * - sc_sequence_callcc accepts any number of functions and executes them in
 *   order. Arguments can be either functions or arrays. If it is a function, it
 *   receives the result of the previous instruction. If it's an array, then the
 *   a[0] must be the fun to be executed, a[1] its 'this' and the remaining elements
 *   its arguments.
 */

/*** META ((export with-handler-lambda)
           (call/cc? #t)
           (call/cc-params (0 1)))
*/
function sc_withHandlerLambda_callcc(handler, body) {
    var sc_storage = SC_CALLCC_STORAGE;
    if (sc_storage['doCall/CcDefault?']) {
    } else if (sc_storage['doCall/CcRestore?']) {
	var _sc_state = sc_storage.pop();
	handler = _sc_state.handler;
	body = sc_storage.getCallNextFunction();
    } else { // root-fun?
	return sc_callCcRoot(this, arguments);
    }
    try {
	return body();
    } catch(e) {
	if (!e._internalException)
	    return handler(e);
	if (e instanceof sc_CallCcException && e.backup) {
	    _sc_state = new Object();
	    _sc_state.callee = sc_withHandlerLambda_callcc;
	    _sc_state.handler = handler;
	    e.push(_sc_state);
	}
	throw e;
    }
}

/*** META ((export apply)
           (call/cc? #t)       
           (call/cc-params (0)))
*/
// no need for _callcc version
var sc_apply_callcc = sc_apply;

/*** META ((export map)
           (call/cc? #t)
           (call/cc-params (0)))
*/
function sc_map_callcc(proc) {
    function map(revres, ls) {
	try {
	    var nbApplyArgs = ls.length;
	    var applyArgs = [];
	    while (ls[0] !== null) {
		for (var i = 0; i < nbApplyArgs; i++) {
		    applyArgs[i] = ls[i].car;
		    ls[i] = ls[i].cdr;
		}
		revres = sc_cons(proc.apply(null, applyArgs), revres);
	    }
	    return sc_reverse(revres);
	} catch(e) {
	    if (e instanceof sc_CallCcException && e.backup) {
		e.pushCont(function(v) {
			return map(sc_cons(v, revres), ls.concat());
		    });
	    }
	    throw e;
	}
    }
    
    if (arguments.length < 2) return null;
    
    var sc_storage = SC_CALLCC_STORAGE;
    if (!sc_storage['doCall/CcDefault?'])
	return sc_callCcRoot(this, arguments);
    var ls = [];
    for (var i = 1; i < arguments.length; i++)
	ls[i-1] = arguments[i];
    return map(null, ls);
}

/*** META ((export map!)
           (call/cc? #t)
           (call/cc-params (0)))
*/
function sc_mapBang_callcc(proc) {
    function mapBang(ls, res) {
	try {
	    var nbApplyArgs = ls.length;
	    var applyArgs = [];
	    while (ls[0] !== null) {
		var tmp = ls[0];
		for (var i = 0; i < nbApplyArgs; i++) {
		    applyArgs[i] = ls[i].car;
		    ls[i] = ls[i].cdr;
		}
		tmp.car = proc.apply(null, applyArgs);
	    }
	    return res;
	} catch(e) {
	    if (e instanceof sc_CallCcException && e.backup) {
		e.pushCont(function(v) {
			tmp.car = v;
			return mapBang(ls.concat(), res);
		    });
	    }
	    throw e;
	}
    }
    
    if (arguments.length < 2) return null;
    
    var sc_storage = SC_CALLCC_STORAGE;
    if (!sc_storage['doCall/CcDefault?'])
	return sc_callCcRoot(this, arguments);
    var ls = [];
    for (var i = 1; i < arguments.length; i++)
	ls[i-1] = arguments[i];
    return mapBang(ls, ls[0]);
}

/*** META ((export for-each)
           (call/cc? #t)
           (call/cc-params (0)))
*/
//
function sc_forEach_callcc(proc, l1) {
    if (!SC_CALLCC_STORAGE['doCall/CcDefault?'])
	return sc_callCcRoot(this, arguments);
    var orig_arguments = arguments;
    try {
	args = new Array(arguments.length - 1);
	while(l1 !== null) {
	    for (var i = 1; i < arguments.length; i++) {
		args[i-1] = arguments[i].car;
		arguments[i] = arguments[i].cdr;
	    }
	    proc.apply(null, args);
	}
    } catch(e) {
	if (e instanceof sc_CallCcException && e.backup) {
	    e.pushCont(function() {
		    sc_forEach_callcc.apply(null, orig_arguments);
		});
	}
	throw e;
    }
}

/*** META ((export filter!)
           (call/cc? #t)
           (call/cc-params (0)))
*/
function sc_filterBang_callcc(proc, l1) {
    function filterBang(tail, next, dummy) {
	try {
	    while (next !== null) {
		if (proc(next.car) !== false) {
		    tail.cdr = next;
		    tail = next;
		}
		next = next.cdr;
	    }
	    tail.cdr = null;
	    return dummy.cdr;
	} catch(e) {
	    if (e instanceof sc_CallCcException && e.backup) {
		e.pushCont(function(v) {
			if (v !== false) {
			    tail.cdr = next;
			    tail = next;
			}
			return filterBang(tail, next.cdr, dummy);
		    });
	    }
	    throw e;
	}
    }

    var sc_storage = SC_CALLCC_STORAGE;
    if (!sc_storage['doCall/CcDefault?']) // root-fun?
	return sc_callCcRoot(this, arguments);

    var dummy = sc_cons("dummy", l1);
    return filterBang(dummy, l1, dummy);
}

/*** META ((export filter)
           (call/cc? #t)
           (call/cc-params (0)))
*/
function sc_filter_callcc(proc, l1) {
    function filter(revres, l) {
	try {
	    while (l !== null) {
		if (proc(l.car) !== false)
		    revres = sc_cons(l.car, revres);
		l = l.cdr;
	    }
	    return sc_reverse(revres);
	} catch(e) {
	    if (e instanceof sc_CallCcException && e.backup) {
		e.pushCont(function(v) {
			if (v !== false)
			    return filter(sc_cons(l.car, revres), l.cdr);
			else
			    return filter(revres, l.cdr);
		    });
	    }
	    throw e;
	}
    }

    var sc_storage = SC_CALLCC_STORAGE;
    if (!sc_storage['doCall/CcDefault?'])
	return sc_callCcRoot(this, arguments);

    return filter(null, l1);
}

/*** META ((export filter-map)
           (call/cc? #t)
           (call/cc-params (0)))
*/
function sc_filterMap_callcc(proc, l1, l2, l3) {
    var map_a = [sc_map_callcc, null];
    for (var i = 0; i < arguments.length; i++)
	map_a[i+2] = arguments[i];
    var filter = function(l) {
	return sc_filterBang(function(x) {
		return x !== false; },
	    l); };
    return sc_sequence_callcc(map_a, filter);
}

/*** META ((export any)
           (call/cc? #t)
           (call/cc-params (0)))
*/
function sc_any_callcc(proc, l) {
    if (!SC_CALLCC_STORAGE['doCall/CcDefault?'])
	return sc_callCcRoot(this, arguments);
    try {
	while (l !== null) {
	    var tmp = proc(l.car);
	    if (tmp !== false) return tmp;
	    l = l.cdr;
	}
	return false;
    } catch(e) {
	if (e instanceof sc_CallCcException && e.backup) {
	    e.pushCont(function(v) {
		    if (v !== false) return v;
		    return sc_any_callcc(proc, l.cdr);
		});
	}
	throw e;
    }
}

/*** META ((export any?)
           (call/cc? #t)
           (call/cc-params (1)))
*/
function sc_anyPred_callcc(proc, l) {
    return sc_sequence_callcc(function() { return sc_any_callcc(proc, l); },
			      function(v) { return v !== false; });
}

/*** META ((export every)
           (call/cc? #t)
           (call/cc-params (0)))
*/
function sc_every_callcc(proc, l) {
    if (!SC_CALLCC_STORAGE['doCall/CcDefault?'])
	return sc_callCcRoot(this, arguments);
    try {
	var tmp;
	while (l !== null) {
	    tmp = proc(l.car);
	    if (tmp === false) return false;
	    l = l.cdr;
	}
	return tmp;
    } catch(e) {
	if (e instanceof sc_CallCcException && e.backup) {
	    e.pushCont(function(v) {
		    if (v === false) return false;
		    if (l.cdr === null) return v;
		    return sc_any_callcc(proc, l.cdr);
		});
	}
	throw e;
    }
}

/*** META ((export every?)
           (call/cc? #t)
           (call/cc-params (1)))
*/
function sc_everyPred_callcc(proc, l) {
    return sc_sequence_callcc(function() { return sc_every_callcc(proc, l); },
			      function(v) { return v !== false; });
}

/*** META ((export force)
           (call/cc? #t)
           (peephole (postfix "()")))
*/
var sc_force_callcc = sc_force;

/*** META ((export call-with-values)
           (call/cc? #t)
           (call/cc-params (0 1)))
*/
function sc_callWithValues_callcc(producer, consumer) {
    var sc_storage = SC_CALLCC_STORAGE;
    if (sc_storage['doCall/CcDefault?']) {
    } else if (sc_storage['doCall/CcRestore?']) {
	var _sc_state = sc_storage.pop();
	consumer = _sc_state.consumer;
	producer = sc_storage.getCallNextFunction();
    } else { // root-fun?
	return sc_callCcRoot(this, arguments);
    }
    try {
	var produced = producer();
    } catch (e) {
	if (e instanceof sc_CallCcException && e.backup) {
	    var _sc_state = new Object();
	    _sc_state.callee = sc_callWithValues_callcc;
	    _sc_state.consumer = consumer;
	    e.push(_sc_state);
	}
	throw e;
    }
    if (produced instanceof sc_Values)
	return consumer.apply(null, produced.values);
    else
	return consumer(produced);
}

var sc_DYNAMIC_WIND_COUNTER = 0; // unique id for each dynamic-wind

/*** META ((export dynamic-wind)
           (call/cc? #t)
           (call/cc-params (0 1 2)))
*/
function sc_dynamicWind_callcc(before, thunk, after) {
    var sc_storage = SC_CALLCC_STORAGE;
    var callCc_tmp;
    if (sc_storage['doCall/CcDefault?']) {
	var res;
	var id = sc_DYNAMIC_WIND_COUNTER++;
	var sc_state = 'before';
    } else if (sc_storage['doCall/CcRestore?']) {
	var _sc_state = sc_storage.pop();
	sc_state = _sc_state.sc_state;
	id = _sc_state.id;
	before = _sc_state.before;
	thunk = _sc_state.thunk;
	after = _sc_state.after;
	res = _sc_state.res;
	caught_exception = _sc_state.caught_exception;
	switch (sc_state) {
	case 'before':
	    before = sc_storage.getCallNextFunction();
	    break;
	case 'during':
	    thunk = sc_storage.getCallNextFunction();
	    break;
	case 'after':
	    after = sc_storage.getCallNextFunction();
	    break;
	case 'exception':
	    var remainingAfter = sc_storage.getCallNextFunction();
	    break;
	}
    } else { // root-fun?
	return sc_callCcRoot(this, arguments);
    }
    try {
	try {
	    switch (sc_state) {
	    case 'before':
		before();
	    case 'during':
		sc_state = 'during';
		res = thunk();
	    case 'after':
		sc_state = 'after';
		after();
		return res;
	    case 'exception':
		remainingAfter();
		throw caught_exception;
	    }
	} catch (e_inner) {
	    if (!(e_inner instanceof sc_CallCcException) &&
		sc_state === 'during') {
		var caught_exception = e_inner;
		sc_state = 'exception';
		after();
	    }
	    throw e_inner;
	}
    } catch (e) {
	if (e instanceof sc_CallCcException && e.backup) {
	    var _sc_state = new Object();
	    _sc_state.callee = sc_dynamicWind_callcc;
	    _sc_state.sc_state = sc_state;
	    _sc_state.id = id;
	    _sc_state.before = before;
	    _sc_state.thunk = thunk;
	    _sc_state.after = after;
	    _sc_state.res = res;
	    _sc_state.caught_exception = caught_exception;
	    e.push(_sc_state);
	}
	if (sc_state === 'during' &&
	    (e instanceof sc_CallCcException)) {
	    e.pushDynamicWind({ id: id, before: before, after: after });
	}
	throw e;
    }
    return undefined; // for FF2.0
}

/*** META ((export #t)
           (call/cc? #t)
           (call/cc-params (1))
           (peephole (jsCall)))
*/
var sc_jsCall_callcc = sc_jsCall;

/*** META ((export js-method-call)
           (call/cc? #t)
           (peephole (jsMethodCall)))
*/
var sc_jsMethodCall_callcc = sc_jsMethodCall;

// HACK: we don't have any call/cc-version of jsNew, as we can't handle it in our call/cc anyways.

/*** META ((export with-input-from-port)
           (call/cc? #t)
           (call/cc-params (1)))
*/
function sc_withInputFromPort_callcc(p, thunk) {
    // no need to verify for "restore...". All operations are safe, and
    // dynamicWind_callcc will handle the rest.
    var tmp = SC_DEFAULT_IN;
    return sc_dynamicWind_callcc(function() { SC_DEFAULT_IN = p; },
				 thunk,
				 function() { SC_DEFAULT_IN = tmp; });
}

/*** META ((export with-input-from-string)
           (call/cc? #t)
           (call/cc-params (1)))
*/
function sc_withInputFromString_callcc(s, thunk) {
    return sc_withInputFromPort_callcc(
	new sc_StringInputPort(sc_string2jsstring(s)),
	thunk);
}

/*** META ((export with-output-to-port)
           (call/cc? #t)
           (call/cc-params (1)))
*/
function sc_withOutputToPort_callcc(p, thunk) {
    // no need to verify for "restore...". All operations are safe, and
    // dynamicWind_callcc will handle the rest.
    var tmp = SC_DEFAULT_OUT;
    sc_dynamicWind_callcc(function() { SC_DEFAULT_OUT = p; },
			  thunk,
			  function() { SC_DEFAULT_OUT = tmp; });
}


/*** META ((export with-output-to-string)
           (call/cc? #t)
           (call/cc-params (0)))
*/
function sc_withOutputToString_callcc(thunk) {
    var p = new sc_StringOutputPort();
    return sc_sequence_callcc(function() { sc_withOutputToPort_callcc(p, thunk); },
			      function() { return p.close(); });
}

/*** META ((export with-output-to-procedure)
           (call/cc? #t)
           (call/cc-params (1)))
*/
/* with-output-to-procedure parameter 'proc' must not call call/cc! */
function sc_withOutputToProcedure_callcc(proc, thunk) {
    var t = function(s) { proc(sc_jsstring2string(s)); };
    return sc_withOutputToPort_callcc(new sc_GenericOutputPort(t), thunk);
}

/*** META ((export hashtable-for-each)
           (call/cc? #t)
           (call/cc-params (1)))
*/
function sc_hashtableForEach_callcc(ht, f) {
    var l = null;
    for (var v in ht) {
	if (ht[v] instanceof sc_HashtableElement)
	    l = new sc_Pair(ht[v], l);
    }
    sc_forEach_callcc(function(el) { f(el.key, el.val); },
		      l);
}

/*** META ((export bind-exit-lambda)
           (call/cc? #t)
           (call/cc-params (0)))
*/
function sc_bindExitLambda_callcc(proc) {
    var sc_storage = SC_CALLCC_STORAGE;
    if (sc_storage['doCall/CcDefault?']) {
	var escape_obj = new sc_BindExitException();
	var escape = function(res) {
	    escape_obj.res = res;
	    throw escape_obj;
	};
    } else if (sc_storage['doCall/CcRestore?'])  {
	var sc_state = sc_storage.pop();
	escape_obj = sc_state.escape_obj;
	proc = sc_storage.getCallNextFunction();
    } else { // root-fun?
	return sc_callCcRoot(this, arguments);
    }
    try {
	return proc(escape);
    } catch(e) {
	if (e === escape_obj) {
	    return e.res;
	}
	if (e instanceof sc_CallCcException && e.backup) {
	    var sc_state = new Object();
	    sc_state.callee = sc_bindExitLambda_callcc;
	    sc_state.proc = proc;
	    sc_state.escape_obj = escape_obj;
	    e.push(sc_state);
	}
	throw e;
    }
}

function sc_CallCcStorage() {
    this['__call/cc-Object'] = true;
    this['doCall/CcDefault?'] = true;
    this.dynamicWinds = [];
}
sc_CallCcStorage.prototype.push = function(state) {
    if (!this.states)
	this.states = new Array();
    this.states.push(state);
};
sc_CallCcStorage.prototype.pop = function() {
    return this.states.pop();
};
sc_CallCcStorage.prototype.peek = function() {
    return this.states[this.states.length - 1];
};
sc_CallCcStorage.prototype.popAll = function() {
    this.states.length = 0;
};
sc_CallCcStorage.prototype.isEmpty = function() {
    return this.states.length == 0;
};
sc_CallCcStorage.prototype.length = function() {
    return this.states.length;
};
sc_CallCcStorage.prototype.duplicate = function() {
    var dupl = new sc_CallCcStorage();
    dupl.states = this.states.concat();
    dupl.rootFun = this.rootFun;
    dupl.rootThis = this.rootThis;
    dupl.dynamicWinds = this.dynamicWinds;
    return dupl;
};
sc_CallCcStorage.prototype.callNext = function() {
    var nextState = this.states[this.states.length-1];
    if (nextState.this_ === SC_TAIL_OBJECT)
	return (0, nextState.callee)(); // use global obj as 'this'.
    else
	return nextState.callee.call(nextState.this_);
};
sc_CallCcStorage.prototype.getCallNextFunction = function() {
    var storage = this;
    return function() {
	return storage.callNext();
    };
};

sc_CallCcStorage.prototype.resetStateIterator = function() {
    this.stateIndex = 0;
};
sc_CallCcStorage.prototype.getNextState = function() {
    return this.states[this.stateIndex++];
};

function sc_CallCcException() {
    this._internalException = true;
    this.storage = new sc_CallCcStorage();
    this.dynamicWinds = [];
};
sc_CallCcException.prototype.push = function(state) {
    this.storage.push(state);
};
sc_CallCcException.prototype.pushCont = function(cont) {
    var state = new Object();
    state.callee = function() {
	var sc_storage = SC_CALLCC_STORAGE;
	var tmp;
	try {
	    var state = sc_storage.pop();
	    tmp = sc_storage.callNext();
	} catch(e) {
	    if (e instanceof sc_CallCcException && e.backup) {
		e.push(state);
	    }
	    throw e;
	}
	return cont(tmp);
    };
    this.storage.push(state);
};
sc_CallCcException.prototype.pushDynamicWind =  function(dynamicWind) {
    this.dynamicWinds.push(dynamicWind);
};

/*** META ((export suspend)
           (call/cc? #t))
*/
function sc_suspend(proc) {
    // the 'return's are completely useless. (just for easier reading).
    sc_callcc(function(resume) {
	try {
	    return proc(resume);
	} finally {
	    return sc_EMPTY_CALLCC("ignored");
	}
    });
}
/*** META ((export call/cc call-with-current-continuation)
           (call/cc? #t))
*/
function sc_callcc(proc) {

    var sc_storage = SC_CALLCC_STORAGE;
    if (sc_storage['doCall/CcDefault?']) {
	/* do nothing */
    } else if (sc_storage['doCall/CcRestore?'])  {
	throw "call/cc can never be used in restoration context";
    } else { // root-fun?
	return sc_callCcRoot(this, arguments);
    }

    var e = new sc_CallCcException();

    // push last state (artificially created).
    var sc_state = new Object();
    // short-cut call/cc and continue directly with "whenRestored".
    // 'whenRestored' will either call 'proc' or return the value (when invoked as continuation).
    sc_state.callee = function() {
	var sc_storage = SC_CALLCC_STORAGE;
	sc_storage['doCall/CcDefault?'] = true;
	sc_storage['doCall/CcRestore?'] = false;
	sc_storage.popAll();
	return sc_storage.whenRestored();
    };
    sc_state.this_ = null;
    e.push(sc_state);

    // we are retrieving all states.
    e.backup = true;

    // just in case the exception is not handled. -> nice error-message.
    e.text = "call/cc-exception_backup";

    // at root call this function
    e.atRoot = function() {
	// backuping is done.
	e.backup = false;

	// backup e.dynamicWinds. these are the dynamicWinds of the stack-frames.
	e.storage.dynamicWinds = e.dynamicWinds;

	// the continuation function...
	var k = function(val) {
	    // 
	    if (!SC_CALLCC_STORAGE['doCall/CcDefault?']) {
		// apparently we are not called out of regular scheme-code.
		// make the best out of it...
		return sc_callCcRoot(null, arguments);
	    }
	    var e_invoke = new sc_CallCcException();
	    e_invoke.text = "call/cc-exception_invocation";
	    e_invoke.atRoot = function() { /* do nothing */ };
	    e_invoke.storage = e.storage; // use the backuped stack-frames.
	    e_invoke.whenRestored = function() { return val; };

	    // throw exception to invoke the continuation
	    throw e_invoke;
	};
	e.whenRestored = function() { return proc(k); };
    };

    // throw e, thus starting the backup.
    throw e;
}

function sc_callCcRestart(e) {
    // fromStack is the dynamic-winds stack from the continuation we are coming from
    // toStack is the dynamic-winds stack we are going to.
    // for both stacks: last element is top-level-dynamic-wind. ie stack[0] is the most nested one.
    // continueFun is the fun we are going to execute once we have executed all the
    // dynamicwinds.
    function dynamicWindsAndContinue(fromStack, toStack, continueFun) {
	var i = fromStack.length - 1;
	var j = toStack.length - 1;
	// find the shared part of dynamic-winds.
	while (i >= 0 &&
	       j >= 0 &&
	       fromStack[i].id === toStack[j].id) {
	    i--;
	    j--;
	}
	
	var toDos = [];
	// 0->i are afters we need to run.
	// j->0 are befores we need to run.    
	for (var k = 0; k <= i; k++) {
	    toDos.push(fromStack[k].after);
	}
	for (var k = j; k >= 0; k--) {
	    toDos.push(toStack[k].before);
	}
	if (toDos.length == 0)
	    return continueFun;
	else
	    toDos.push(continueFun);
	
	return function() {
	    var sc_storage = SC_CALLCC_STORAGE;
	    if (sc_storage['doCall/CcDefault?']) {
		var i = 0;
		var f = toDos[i];
	    } else if (sc_storage['doCall/CcRestore?'])  {
		var sc_state = sc_storage.pop();
		i = sc_state.i;
		f = sc_storage.getCallNextFunction();
	    } else { // root-fun?
		return sc_callCcRoot(this, arguments);
	    }
	    try {
		do {
		    var res = f();
		    i++;
		    f = toDos[i]; // in JS we can access elements out of boundaries.
		} while (i < toDos.length);
		return res;
	    } catch (e) {
		if (e instanceof sc_CallCcException && e.backup) {
		    var sc_state = new Object();
		    sc_state.callee = arguments.callee;
		    sc_state.i = i;
		    e.push(sc_state);
		}
		throw e;
	    }
	};
    }
	
    while (true) {
	try {
	    e.atRoot();
	    // always duplicate the storage.
	    // (each restored function will pop its state).
	    var sc_storage = e.storage.duplicate();

	    // we have a new storage...
	    SC_CALLCC_STORAGE = sc_storage;
	    SC_SCM2JS_GLOBALS.CALLCC_STORAGE = SC_CALLCC_STORAGE;

	    // once restored executed the dynamic winds first.
	    sc_storage.whenRestored = dynamicWindsAndContinue(e.dynamicWinds,
							      sc_storage.dynamicWinds,
							      e.whenRestored);
	    sc_storage['doCall/CcDefault?'] = false;
	    sc_storage['doCall/CcRestore?'] = true;
	    // and GO!
	    return sc_storage.callNext();
	} catch (e2) {
	    if (e2 instanceof sc_CallCcException) {
		e = e2;
	    } else
		throw e2;
	}
    }
}

var SC_CALLCC_STORAGE = new Object();
SC_CALLCC_STORAGE['doCall/CcDefault?'] = false;
SC_CALLCC_STORAGE['doCall/CcRestore?'] = false;
SC_CALLCC_STORAGE['__call/cc-Object'] = true;
SC_SCM2JS_GLOBALS.CALLCC_STORAGE = SC_CALLCC_STORAGE;

function sc_callCcRoot(rootThis, rootArguments) {
    var tmp = SC_CALLCC_STORAGE;
    try {
	SC_CALLCC_STORAGE = new sc_CallCcStorage();
	SC_SCM2JS_GLOBALS.CALLCC_STORAGE = SC_CALLCC_STORAGE;
	return rootArguments.callee.apply(rootThis, rootArguments);
    } catch (e) {
	if (e instanceof sc_CallCcException) {
	    return sc_callCcRestart(e);
	} else
	    throw e;
    } finally {
	SC_CALLCC_STORAGE = tmp;
	SC_SCM2JS_GLOBALS.CALLCC_STORAGE = SC_CALLCC_STORAGE;
    }
	
}

var sc_EMPTY_CALLCC;
sc_callcc(function(k) { sc_EMPTY_CALLCC = k; });


var sc_INDEX_OBJECT_HASH = new Array;

function sc_getCallCcIndexObject(index) {
    if (sc_INDEX_OBJECT_HASH[index])
	return sc_INDEX_OBJECT_HASH[index];
    else {
	var tmp = { index: index };
	sc_INDEX_OBJECT_HASH[index] = tmp;
	return tmp;
    }
}

function sc_sequence_callcc() {
    function sc_seq(lastVal, i, funs) {
	var sc_storage = SC_CALLCC_STORAGE;
	if (!sc_storage['doCall/CcDefault?'])
	    return sc_callCcRoot(this, arguments);

	try {
	    while (true) {
		if (i >= funs.length)
		    return lastVal;
		var f = funs[i];
		i++;
		if (typeof f === 'function')
		    lastVal = f(lastVal);
		else
		    lastVal = f[0].apply(f[1], f.slice(2));
	    }
	} catch(e) {
	    if (e instanceof sc_CallCcException && e.backup) {
		e.pushCont(function(v) {
			return sc_seq(v, i, funs);
		    });
	    }
	    throw e;
	}
    }
    return sc_seq(undefined, 0, arguments);
}
    
