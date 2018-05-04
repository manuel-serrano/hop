
Building the docker image
=========================

  (cd docker; docker build -t hop .)

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
  docker run -t --entrypoint=/bin/bash -v /tmp/hopdocker:/hoproot hop


Extract the documentation
-------------------------

Run a Hop container image and execute:

  docker cp hop:/usr/local/share/doc/hop/3.2.0 .


Extract the hop.docker
----------------------

Run a Hop container image and execute:

  docker cp hop:/tmp/hop/3.2.0/docker/hop.docker .

Example
=======

$ mkdir /tmp/hopdocker
$ cp -r examples /tmp/hopdocker
$ docker run -d -p 8080:8080 -v /tmp/hopdocker:/hoproot hop /hoproot/examples/examples/examples.js --libs-dir /hoproot/libs
