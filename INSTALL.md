### Prerequisites ###

The Hop system requires at a minimum a Linux or OSX environment and
the standard suite of compilation tools. 

  - gcc or clang (on OSX, use the latest version of Xcode)

We recommend that you also install the following additional tools
using your prefered package manager in order to get best results
during the automatic configuration phase of bigloo and hop software:

  - autoconf
  - automake
  - curl
  - pkg-config
  - libtool
  - sqlite3

At configuration time, the system checks the availability of
additional libraries. Supported libraries include:

  - openssl (install openssl-devel)
  - gmp (install gmp-devel)
  - sqlite (install sqlite-devel)
  - phidgets (only if you use Hop to control Phidget hardware sensors and
  actuators)


#### Prerequisites for OSX ####

Hop.js 3.0.x is supported on recent versions of OSX, including
Yosemite (10.10) and El Capitan (10.11), and may also work on older OS
versions.

A convenient way to install the above tools and libraries on OSX is to
use macports. See <https://www.macports.org> to install and use
macports. Once macports is installed, you may for example install
openssl simply by typing: sudo port install openssl

We recommend that you install the following packages (some of them are
required to get full functionality or enhanced compatibility with Node.js):

  - autoconf
  - automake
  - cmake
  - curl
  - gawk
  - getopt
  - giflib
  - gmp
  - libtool
  - libunistring
  - openssl
  - sqlite3

Important notice: the openssl version shiped with OSX is too old and
deprecated by Apple, please install openssl 1.0.2 or a newer version.

Tools and libs installed using *macports* are located in
`/opt/local/{bin,lib}`.  Macports updates the `PATH` environment
variable to look for executable files into `/opt/local/bin` but does not
change other env. variables. You must set `C\_INCLUDE\_PATH` and
`LIBRARY\_PATH` in your `.profile` file in order for the bigloo
autoconfiguration tool to detect and use libraries installed in
`/opt/local`.


    export C_INCLUDE_PATH=/opt/local/include 
    export LIBRARY_PATH=/opt/local/lib

Some of the installed libraries come with a pkg-config support
file. Add this directory to `PKG\_CONFIG\_PATH`, again to have the
autocofiguration tool work as expected.

    export PKG_CONFIG_PATH=/opt/local/lib/pkgconfig:/usr/lib/pkgconfig


The setting for *brew* is

    export C_INCLUDE_PATH=$(brew --prefix)/opt/openssl/include:$C_INCLUDE_PATH
    export LIBRARY_PATH=$(brew --prefix)/opt/openssl/lib:$LIBRARY_PATH
    export PKG_CONFIG_PATH=$(brew --prefix)/opt/openssl/lib/pkgconfig

If you use another package manager such fink, or if you install
libraries from sources, please change above paths and install commands
accordingly.

### Install bigloo ###

Hop.js requires that bigloo (a scheme compiler and runtime) is
installed on the target machine. bigloo can be downloaded from
<ftp://ftp-sop.inria.fr/indes/fp/Bigloo/bigloo4.2c.tar.gz> (or a newer
version).

    tar xvf bigloo4.2c.tar.gz

and cd to the build directory (bigloo4.2c) .

Run the configure script:

    ./configure

Result of the automatic configuration script is logged in the file
configure.log . You may find there the list of libraries that were
found and bigloo configuration parameters. The default installation
path is `/usr/local/{lib,bin,...}`. It can be overhidden with the
`--prefix` option :

    ./configure --prefix=<custom_installation_path>

Other configuration options are listed by typing:

    ./configure --help

then run 

     make

(or `make -j` to run build tasks in parallel on multicore machines)

At last run

     make install
or
     sudo make install

depending on your access rights on the installation directory.


### Install hop ###

Untar the hop archive into a fresh directory and cd to the root
directory. Run the configure script:

    ./configure

The `--prefix` option works as usual to set a custom <install-prefix>
(default is `/usr/local`).

An other interesting option is:

--bigloo=<bigloo\_path> to specify a specific bigloo executable to be
  used by hop. The default value will retrieve bigloo in the user PATH.

    ./configure --help

to see all available options. Once the configuration is completed, run :

     make (or make -j)

then run

     make install (or  sudo make install)


#### Test the hop.js build ####

    cd test/hopjs
run

    ./TEST

in order to run the hopjs test suite.

### Documentation ###

An html documentation can be generated from source markdown files 
using hop.js:

    cd doc && make && sudo make install 

The main documentation file is `<share-dir>/doc/hop/index.html` where 
<share-dir> defaults to `/usr/local/share`.


### Set startup preferences and run hop ###

Make sure that your `$PATH` variable includes the bin install
directory (which defaults to `/usr/local/bin`).


By default Hop.js only accepts to serve authenticated requests. Before
executing any programs users must be declared. These declarations go
into the `$HOME/.config/hop/hoprc.js` file. The following declare a user
named hopjs whose password is inria and that is allowed to execute any
Hop.js service, the declaration services: "*", and download any file
readable from the server process, the declaration directories: "*":

    mkdir -p $HOME/.config/hop && cat > $HOME/.config/hop/hoprc.js << EOF
    hop = require( "hop" );
    var user = require( hop.user );
    var config = require( hop.config );

    user.add( { name: "hopjs",
                password: user.encryptPassword( "hopjs", "inria" ),
                services: "*",
                directories: "*"
              } );
    EOF

Then start hop by running:

    hop

hop starts the builtin web server on the default 8080 port. You may
define an alternative port by typing instead the command:

    hop -p <port>

Open your browser to the builtin web server:
<http://localhost:8080/hop> (or <http://localhost:PORT/hop> )


You may also enforce the use of a custom preferences files using
the `--hoprc` option.

    hop --rc-file myrcfile.js


### Run examples ###

    cd <hop-build-dir>

then run

    hop examples/examples/examples.js

and open a browser on http://localhost:8080/hop/examples to play with
hop.js examples.


### Edit source code ###

hop.js syntax is a superset of JavaScript.

Most source files can be edited with a JavaScript editor.
Some editors may have trouble with the extended syntax :
 the service keyword,
 the ~{} and ${} blocks,
 the <> tags.

First, look at your editor preferences for a possible support of non
standard extensions. Emacs is known to provide extensive support for
custom extensions (an emacs js mode hook file is provided as part of
the hop.js distribution).

If you are still in trouble, you may split your code between standard
node.js files and hop.js files, turning off the electric mode on files
that use hop.js extensions.


### Troubleshoot installation issues ###

Most common issues:

  - Missing libraries/ libraries not found. Check prerequisites in
  section 1. bigloo/configure.log provides information about which
  libraries were found and which ones could not be found.  Specific
  configuration steps can be run by hand. For example:
  
    <bigloo-source>/autoconf/runtest -v10 -- bgl-pkg-config "--libs" "openssl"

  would run the autoconfiguration test for openssl in verbose mode.
  Tools like `ldd` (or `otool -L` on OSX) are useful to track libraries
  issues.
	
  - corrupted source directory.  Avoid copying an already configured or
  compiled source  tree from another machine. Make sure to run make
  cleanall after you pull hop.js updates from github. It is always safer
  to start with a fresh source directory to build bigloo or hop.
	
  - wrong user preference files. Many things have changed since
  hop-2.5.x. We advise you to delete the ~/.config/hop directory where
  user preferences and cached data are stored, then follow the startup
  procedure once again.

