
Building and exporting the docker image
=======================================

  (cd docker; docker build -t hop .)
  docker image save -o "/tmp/hop-docker-image-`date '+%d%b%y'`-`(git rev-parse --short HEAD)`.tgz" hop
  docker export -o "/tmp/hop-docker-`date '+%d%b%y'`-`(git rev-parse --short HEAD)`.tgz"

It might be needed to first remove an existing Hop image with:

  docker images

to get the Hop docker <image-id>, and then:

  docker rmi <image-id>


Importing a docker image
========================

The Hop web site contains a pre-built docker image. To import it, use
the following:

  docker load < hop-docker-image-4may2018.tgz
  docker import hop-docker-4may2018.tgz -- hop
  

Running the image
=================

Run as deamon
-------------
  docker run -d -p 8080:8080 -v /tmp/hopdocker:/hoproot hop /hoproot/app.js --libs-dir /hoproot/libs

Run in the console
------------------
  docker run -a STDIN -a STDERR -a STDOUT --tty -p 8080:8080 -v /tmp/hopdocker:/hoproot hop /hoproot/app.js --libs-dir /hoproot/libs

Run a shell
----------
  docker run -t -i --entrypoint=/bin/bash -v /tmp/hopdocker:/hoproot hop

Extract the documentation
-------------------------

Run a Hop container image and execute:

  docker cp hop:/usr/local/share/doc/hop/3.2.0 .

Extract the hop.docker
----------------------

Run a Hop container image and execute:

  docker cp hop:/tmp/hop/3.2.0/docker/hop.docker .

Example with hop.docker
=======================

$ mkdir -p $HOME/.config/hop
$ cat > $HOME/.config/hop/hoprc.js << EOF
hop = require( "hop" );
var user = require( hop.user );
user.add( { name: "anonymous", services: "*", directories: "*" } );
EOF
$ cat > /tmp/hello.js << EOF
console.log( "Hello World!" );

service hello() {
  return <html>Hello World!</html>'
}
EOF
$ hop.docker /tmp/hello.js
$ firefox http://localhost:8080/hop/hello

Example without hop.docker
==========================

$ mkdir /tmp/hopdocker
$ cp -r examples /tmp/hopdocker
$ docker run -d -p 8080:8080 -v /tmp/hopdocker:/hoproot hop /hoproot/examples/examples/examples.js --libs-dir /hoproot/libs
