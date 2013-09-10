/*=====================================================================*/
/*    serrano/prgm/project/hop/2.5.x/share/hop-history.js              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Sep 20 07:59:42 2007                          */
/*    Last change :  Sat Aug 17 19:43:38 2013 (serrano)                */
/*    Copyright   :  2007-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP history manager.                                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_state_history_handler ...                                    */
/*---------------------------------------------------------------------*/
var hop_current_state_history = undefined;
var hop_state_history_handler = {};

/*---------------------------------------------------------------------*/
/*    hop_state_history_register_handler ...                           */
/*---------------------------------------------------------------------*/
function hop_state_history_register_handler( key, reset, proc ) {
   hop_state_history_handler[ key ] = { reset: reset, proc: proc };
   hop_eval_history_state( true );
}

/*---------------------------------------------------------------------*/
/*    _hop_state_entry ...                                             */
/*    -------------------------------------------------------------    */
/*    Private class.                                                   */
/*---------------------------------------------------------------------*/
function _hop_state_entry( op, val ) {
   this.op = op;
   this.val = val;
   this.close = false;
}

/*---------------------------------------------------------------------*/
/*    hop_state_history_to_location ...                                */
/*---------------------------------------------------------------------*/
function hop_state_history_to_location( state ) {
   var loc = undefined;
   
   for( var p in state ) {
      if( state[ p ] instanceof _hop_state_entry ) {
	 if( loc == undefined ) {
	    loc = "#" + p + "=" + state[ p ].op + ":" + state[ p ].val;
	 } else {
	    loc += "," + p + "=" + state[ p ].op + ":" + state[ p ].val;
	 }
      }
   }

   return loc;
}

/*---------------------------------------------------------------------*/
/*    hop_hash_history_regexp ...                                      */
/*---------------------------------------------------------------------*/
var hop_hash_history_regexp = /#?([^=]+)=([^:]+):([^,]+)+/;

/*---------------------------------------------------------------------*/
/*    hop_location_to_state_history ...                                */
/*---------------------------------------------------------------------*/
function hop_location_to_state_history( hash ) {
   var state = {};
   var split = hash.split( "," );
   for( var i = 0; i < split.length; i++ ) {
      var el = split[ i ].match( hop_hash_history_regexp );
      if( el ) {
	 var id = el[ 1 ];
	 var op = el[ 2 ];
	 var val = el [ 3 ];

	 state[ id ] = new _hop_state_entry( op, val );
      }
   }

   return state;
}

/*---------------------------------------------------------------------*/
/*    hop_state_history_push ...                                       */
/*    -------------------------------------------------------------    */
/*    Store the new state but don't add a new location to the          */
/*    browser URL bar.                                                 */
/*---------------------------------------------------------------------*/
function hop_state_history_push( id, op, val ) {
   if( hop_current_state_history == undefined ) {
      /* create a new state */
      hop_current_state_history = {};
      hop_current_state_history[ id ] = new _hop_state_entry( op, val );
   } else {
      /* update the current state */
      var olde = hop_current_state_history[ id ];

      if( olde == undefined ) {
	 /* add a new entry to the current state */
	 hop_current_state_history[ id ] = new _hop_state_entry( op, val );
      } else {
	 if( (olde.op != op) || (olde.val != val) ) {
	    /* update the current state */
	    olde.op = op;
	    olde.val = val;
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_state_history_flush ...                                      */
/*    -------------------------------------------------------------    */
/*    Generates a new browser URL from the current history state.      */
/*---------------------------------------------------------------------*/
function hop_state_history_flush() {
   /* store the new state as a location for bookmarking an history */
   var loc = hop_state_history_to_location( hop_current_state_history );
   var old = window.location.href;
   var i = old.indexOf( "#" );

   /* store the new browser URL */
   if( i == -1 ) {
      hop_hashchange_set( document, old + loc );
   } else {
      hop_hashchange_set( document, old.substring( 0, i ) + loc );
   }
}
   
/*---------------------------------------------------------------------*/
/*    hop_state_history_transaction ...                                */
/*---------------------------------------------------------------------*/
var hop_state_history_transaction = 0;

/*---------------------------------------------------------------------*/
/*    hop_state_history_add ...                                        */
/*---------------------------------------------------------------------*/
function hop_state_history_add( id, op, val ) {
   /* prepare the new current state */
   hop_state_history_push( id, op, val );

   if( hop_state_history_transaction == 0 ) {
      hop_state_history_flush();
   }
}

/*---------------------------------------------------------------------*/
/*    hop_with_history ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export with-history) (arity #t)) */
function hop_with_history( proc ) {
   var res;
   hop_state_history_transaction++;
   try {
      res = proc();
   } finally {
      hop_state_history_transaction--;
   }
   hop_state_history_flush();
   return res;
}
   
/*---------------------------------------------------------------------*/
/*    hop_state_history_reset ...                                      */
/*    -------------------------------------------------------------    */
/*    When there is already an existing state, we have to reset all    */
/*    its entries.                                                     */
/*---------------------------------------------------------------------*/
function hop_state_history_reset() {
   if( hop_current_state_history != undefined ) {
      /* there is a state, we reset all the entries */
      for( p in hop_current_state_history ) {
	 if( hop_current_state_history[ p ] instanceof _hop_state_entry ) {
	    var op = hop_current_state_history[ p ].op;
	    var handler =  hop_state_history_handler[ op ];
	    if( handler != undefined ) {
	       handler.proc( p, handler.reset );
	    }
	 }
      }

      /* and we erase the state itself */
      hop_current_state_history = undefined;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_state_history_update ...                                     */
/*    -------------------------------------------------------------    */
/*    Compare the two states, reset the entries of the old ones        */
/*    that are no longer present in the new one. Execute the           */
/*    entries that are novel in the new state.                         */
/*    -------------------------------------------------------------    */
/*    This function returns the number of entries that have not        */
/*    been correctly updated.                                          */
/*---------------------------------------------------------------------*/
function hop_state_history_update( olds, news ) {
   var res = 0;

   if( olds === undefined ) {
      /* set the new values */
      for( var p in news ) {
	 var state = news[ p ];
	 if( state instanceof _hop_state_entry ) {
	    var op = state.op;
	    var handler = hop_state_history_handler[ op ];

	    if( (handler != undefined) && !state.close ) {
	       if( handler.proc( p, state.val ) ) {
		  state.close = true;
	       } else {
		  res++;
	       }
	    }
	 }
      }
   } else {
      /* reset all the entries that used to be in old    */
      /* state that are no longer present in the new one */
      for( p in olds ) {
	 if( (olds[ p ] instanceof _hop_state_entry) &&
	     !(news[ p ] instanceof _hop_state_entry) ) {
	    var op = olds[ p ].op;
	    var handler = hop_state_history_handler[ op ];

	    if( handler != undefined ) {
	       handler.proc( p, handler.reset );
	    }
	 }
      }

      /* update all the entries that are not */
      /* present and equal in old state      */
      for( p in news ) {
	 var state = news[ p ];
	 if( state instanceof _hop_state_entry ) {
	    if( !(olds[ p ] instanceof _hop_state_entry) ||
		(state.op != olds[ p ].op) ||
		(state.val != olds[ p ].val) ) {
	       var op = state.op;
	       var handler = hop_state_history_handler[ op ];

	       if( (handler != undefined) && !state.close ) {
		  if( handler.proc( p, state.val ) ) {
		     state.close = true;
		  } else {
		     res++;
		  }
	       }
	    }
	 }
      }
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    hop_hash_history_check_regexp ...                                */
/*---------------------------------------------------------------------*/
var hop_hash_history_check_regexp = new RegExp( "^#(?:[^=]+=[^:]+:[^,]+,?)+$" );

/*---------------------------------------------------------------------*/
/*    hop_hash_historyp ...                                            */
/*    -------------------------------------------------------------    */
/*    Is a hash value a legal Hop history?                             */
/*---------------------------------------------------------------------*/
function hop_hash_historyp( hash ) {
   return hop_hash_history_check_regexp.exec( hash );
}

/*---------------------------------------------------------------------*/
/*    hop_current_history ...                                          */
/*---------------------------------------------------------------------*/
/*** META ((export current-history) (arity #t)) */
function hop_current_history() {
   var hash = location.hash;

   if( hash.length == 0 ) {
      return false;
   }

   if( hop_hash_historyp( hash ) ) {
      return hop_location_to_state_history( hash );
   }

   return false;
}

/*---------------------------------------------------------------------*/
/*    hop_replay_history ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export replay-history) (arity #t)) */
function hop_replay_history( hist ) {
   hop_current_state_history = undefined;
   var loc = function( v ) { this.hash = v; }
   hop_eval_history_state( new loc( hop_state_history_to_location( hist ) ) );
}
   
/*---------------------------------------------------------------------*/
/*    _hop_history ...                                                 */
/*    -------------------------------------------------------------    */
/*    Private constructor.                                             */
/*---------------------------------------------------------------------*/
function _hop_history( key ) {
   this.key = key;
}

/*---------------------------------------------------------------------*/
/*    hop_make_history ...                                             */
/*    -------------------------------------------------------------    */
/*    This is the high level constructor presented to the Hop          */
/*    API.                                                             */
/*---------------------------------------------------------------------*/
/*** META ((export make-history) (arity -3)) */
function hop_make_history( key, handler, reset ) {
   hop_state_history_register_handler( key, reset, handler );
   return new _hop_history( key );
}

/*---------------------------------------------------------------------*/
/*    hop_history_add ...                                              */
/*    -------------------------------------------------------------    */
/*    This high level function for adding an entry into the history.   */
/*---------------------------------------------------------------------*/
/*** META ((export history-add!) (arity #t)) */
function hop_history_add( history, id, val ) {
   if( !history instanceof _hop_history ) {
      alert( "*** ERROR: Illegal history object -- " + history );
      return false;
   } else {
      return hop_state_history_add( id, history.key, val );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_eval_history_state ...                                       */
/*    -------------------------------------------------------------    */
/*    This function is invoked when the location has changed.          */
/*---------------------------------------------------------------------*/
function hop_eval_history_state( _ ) {
   var hash = location.hash;

   if( hash.length == 0 ) {
      hop_state_history_reset();
   } else {
      if( hop_hash_historyp( hash ) ) {
	 var new_state = hop_location_to_state_history( hash );
	 var old_state = hop_current_state_history;
	 var count = hop_state_history_update( old_state, new_state );

	 if( count == 0 ) {
	    /* the update is complete, we state the new state and exit */
	    hop_current_state_history = new_state;
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    Install the location event listener                              */
/*---------------------------------------------------------------------*/
if( !hop_config.history ) {
   hop_config.history = true;
   hop_add_event_listener(
      window, "ready",
      function( e ) {
	 hop_add_event_listener( window, "hashchange", hop_eval_history_state );
      } );
}

