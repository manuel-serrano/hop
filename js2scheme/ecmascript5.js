const hop = ((function() {
   const proto = {
      Yield: function( val, kont ) {
	 this.next = kont;
	 return { value: val, done: false }
      },
      Return: function( val, kont ) {
	 return { value: val, done: true }
      },
      YieldS: function( val, done, kont ) {
	 var gen = this;
	 function loop( v, e ) {
	    var n = val.next();
	    
	    if( n.done ) {
	       return kont( n.value, false );
	    } else {
	       return gen.Yield( n.value, false, loop );
	    }
	 }
	 
	 return loop( undefined, undefined );
      },
      kid: function( n, exn ) {
	 return { value: undefined, done: true };
      }
   }
      
   function gen( proc ) {
      this.next = proc;
      this.throw = function( exn ) { return this.next( exn, true ) };
   }

   gen.prototype = proto;

   return { Generator: gen }
})());
