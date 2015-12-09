const hop = ((function() {
   const proto = {
      yield: function( val, done, kont ) {
	 this.next = kont;
	 return { value: val, done: done }
      },
      kid: function( n ) {
	 return { value: undefined, done: true };
      }
   }
      
   function gen( proc ) {
      this.next = proc;
      this.done = false;
   }

   gen.prototype = proto;

   return { Generator: gen }
})());
