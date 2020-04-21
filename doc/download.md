${ var doc = require( "hopdoc" ) }
${ var config = require( hop.config ) }
${ var xml = require( doc.BUILDDIR + "/doc/xml.js" ) }
${ var cfg = require( doc.BUILDDIR + "/doc/doc.json" ) }

## License ##

${doc.include( "./license.md" )}

## Binary distributions ##

Precompiled Hop.js distributions are available.

### Debian/Raspberry ###

A Debian/Raspberry repository is available. To use it, add the following
to your `apt` path:

     deb ftp://ftp-sop.inria.fr/indes/fp/Hop/debian squeeze hop


## Source code installation ##

${<div class="row">
  <div class="col-xs-8">
This is the file you should download if you want to build the Hop stable
version from the sources.
  </div>
  <div class="col-xs-4">
    <xml.downloadButton
       class="warning"
       title="Stable"
       icon="glyphicon-download"
       href=${cfg.urlbase + "/hop-" + cfg.version + ".tar.gz"}/>
  </div>
</div>}

${<div class="row">
  <div class="col-xs-8">
Compiling Hop.js requires the Bigloo compiler.
  </div>
  <div class="col-xs-4">
    <xml.downloadButton
       class="danger"
       title="Stable"
       icon="glyphicon-download"
       href=${cfg.bglurlbase + "/bigloo-" + cfg.bglversion + ".tar.gz"}/>
  </div>
</div>}

#### Bigloo installation ####

To configure and install the Bigloo compiler that is needed to compile
Hop, use the following:

```shell
./configure --abort-missing && make && sudo make install
```

#### Hop installation ####

To configure and install Hop, execute the following:

```shell
./configure && make && sudo make install
```

Optionally, to compile and install the documentation:

```shell
make doc && sudo make install
```

To test the installation:

```shell
make test
```


## Docker installation ##

${<div class="row">
  <div class="col-xs-8">
The recommanded way to install and run Hop is to use
<a href="https://docs.docker.com/install/">Docker</a>. Download the
<tt>${"hop-" + cfg.version + ".dockerfile"}</tt> script and execute the
following command to generate the Docker image:
  </div>
  <div class="col-xs-4">
    <xml.downloadButton
       class="primary"
       title="Stable"
       icon="glyphicon-download"
       href=${cfg.urlbase + "/hop-" + cfg.version + ".dockerfile"}/>
  </div>
</div>}


`docker build -f hop-${cfg.version}.dockerfile -t hop .`

${ <span class="label label-warning">Note:</span> } If you already have
installed Hop within docker, you might find useful to remove the old
image first. This can be achieved with:

```shell
$ docker container prune
$ docker rmi `docker images | grep hop | awk '{print $3}'`
```


### Running Hop with Docker ###

${<div class="row">
  <div class="col-xs-8">
How to run Hop inside docker depends on the Host platform and the local
Docker configuration. In particular, the mapping between Docker network
interfaces and the host network interfaces is system dependant. With
Linux and MacOSX, Hop inside Docker should be accessible via
<tt>localhost</tt>. On Windows, it might be needed to use the IP address generated
automatically when its daemon is started.
  </div>
  <div class="col-xs-4">
    <xml.downloadButton
       class="primary"
       title="Stable"
       icon="glyphicon-download"
       href=${cfg.urlbase + "/hop-" + cfg.version + ".docker"}/>
  </div>
</div>}

The bash script `hop-${cfg.version}.docker` makes running Hop inside
Docker almost as simple as running it natively as it automatically
associates the host disk and the Docker image disk as needed by the
Hop application. For instance, it can be used as:

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
    $ hop-${cfg.version}.docker -p 8888 /tmp/hello.js
    $ firefox http://localhost:8888/hop/hello

${ <span class="label label-warning">Note:</span> } On Windows,
`hop-${cfg.version}.docker` has to be executed from within the Bash
shell script spawned by Docker.

The `hop${cfg.version}.docker` file is shipped with the Docker image.
In addition to be downloaded from the Hop ftp repository, it can be
copied to the local disk with:

`docker cp hop:/usr/local/share/hop/${cfg.branch}/hop.docker .`

### Accessing the documentation and the Emacs mode ###

The complete Hop documentation is embedded inside the Docker image in the
directory:

`/usr/local/share/doc/hop/${cfg.version}`

It can be copied to the host disk with:

`docker cp hop:/usr/local/share/doc/hop/${cfg.branch} .`

It can also be accessed via Hop. For instance, on a Linux host, if Hop
is executed with

`hop.docker -p 9999`

the documentation could be access at the URL:

`http://localhost:9999/usr/local/share/doc/hop/${cfg.version}/index.html`

The Emacs mode can be copied to the host disk with:

`docker cp hop:/usr/local/share/hop/site-lisp/hopjs.el .`

## Git ##

Hop.js can be forked at

${<a href=${cfg.github}>${cfg.github}</a>}


## Older releases ##

Old Hop releases can be obtain at the following address:

<ftp://ftp-sop.inria.fr/indes/fp/Hop/>


## Complete Installation Guide ##


${doc.include( "../INSTALL.md" )}
