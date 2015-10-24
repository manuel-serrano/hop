var proto = Object.create( HTMLElement.prototype );

proto.createdCallback = function() {
   this.innerHTML = 'Hello, <b>' +
      (this.getAttribute('name') || '?') + '</b>';
};

document.registerElement( 'foo-bar', { prototype: proto } );

alert( "glop" );
