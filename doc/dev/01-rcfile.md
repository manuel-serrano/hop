${var hop = require( "hop" )}
${var hopdoc = require( "hopdoc" )}


### RC file ###

When Hop starts (_i.e.,_ the `hop` command is executed), it first
reads a *Runtime Command File* that configures its runtime environment.
The most important purpose of the file is to declare the users that will
be allowed to access the Hop services. The default name of this file
is `$HOME/.config/hoprc.js`, which can be changed using command line
options (see [command](./00-command.html)).

The `hoprc.js` is a regular JavaScript file. It can import any
other modules or library. However, it has special rights that let it
execute expressions that will be later on forbidden or ineffective.
In particular, the `hoprc.js` is the only file where the function
`user.add` (see [user](./user.html)) can be called.

For the example sake here is how to declare an `hoprc.js` with two
users `hopjs` and `anonymous`.

```hopscript
hop = require( "hop" );
var user = require( hop.user );
var config = require( hop.config );

user.add( { name: "hopjs",
            password: user.encryptPassword( "hopjs", "inria" ),
            services: "*",
            directories: "*"
          } );
user.add( { name: "anonymous",
            password: user.encryptPassword( "hopjs", "23^73++_*" ),
            services: "",
            directories: ""
          } );	  
```

${ <span class="label label-warning">Note:</span> } The anonymous user
is the user on which all non-authentified request are
identified. Declaring an anonymous user in the ++hoprc.js++ file is
mandatory. Without such a declaration, the server will raise an error
for each new web request it will receive. The anonymous might have no
permissions granted, as in the example above, but it must
exist. [:@warning]

The `rcfile.js` can be generated automatically using the Hop
wizard (`http://localhost:8080/hop/wizard`).
