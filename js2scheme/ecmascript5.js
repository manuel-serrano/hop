const hop = ((function() {
   const proto = {
      yield: function( val, done, kont ) {
	 this.next = kont;
	 return { value: val, done: done }
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
