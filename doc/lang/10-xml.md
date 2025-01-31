${ var doc = require( "@hop/hopdoc" ) }

XML
===

The XML syntax is part of the Hop.js language (see [syntax](./00-syntax.html)).
In Hop.js, an XML tag is just an alternative syntax for a function call. The
name of the function to be called is the uppercase translation of the
tag name, the tag attributes are all packed into a JavaScript object
that is passed as first argument. The tag children are then passed as
rest argument to the function. For instance, the expression:

```hopscript
<mytag id=t1 cnt=302>a text</mytag>
```

is equivalent to:

```hopscript
MYTAG( { id: t1, cnt: 302 }, "a text" )
```

In a tag children, the `\${...}` form are evaluated, as well as 
other tag calls. For instance,  

```hopscript
<hour>current time: <strong>${Date.now}</strong>.</hour>
```

is equivalent to:

```hopscript
HOUR( {}, "current time: ", STRONG( {}, Date.now ), "." )
```


Custom Tags
-----------

A consequence of the XML syntax equivalence is that any JavaScript
function can be invoked using the XML syntax, provided its name is
uppercased. This can be used to define new HTML abstraction, for instance
a new HTML `center` tag:

```hopscript
"use strict";

function CENTER( attr, ... nodes ) {
   const style = "margin-left: auto; margin-right: auto";
   if( "style" in attr ) { 
      attr.style += ";" + style;
   } else {
      attr.style = style;
   }
   
   nodes.unshift( attr );
   return DIV.apply( undefined, nodes );
}

service svc() {
   return <html>
     <div style="width:800px">
       <center style="width: 100px"> toto tata</center>
     </div>
   </html>
}
```

Custom tags are not restrict to construct HTML trees. They can be
used as any regular functions. Example:

```hopscript
"use strict";

function FIB( { value: value }, ... nodes ) {
   if( value < 2 ) {
      return 1;
   } else {
      return FIB( { value: value - 1 } ) + FIB( { value: value - 2 } );
   }
}

console.log( <FIB value=20/> );
```

XML tag can also be conveniently used to build data structures. Example:

```hopscript
"use strict"

function BOX(attr, ... children ) {
   return { type : "BOX", attr : attr, children: children };
}

function ELEM( attr, ... children ) {
   return { type : "Elem",  attr : attr, children: children };
}

console.log( <box id="1"> <elem> 1 </elem> <elem> 2 </elem> <elem> 3 </elem> </box> );
console.log( <box id="2"> ${[1, 2, 3].map( ELEM )} </box> );
```
