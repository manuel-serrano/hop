#! /bin/bash

set -e

export PATH=/usr/local/bin:$PATH

if [ $# -lt 2 ]; then
   echo "you should not call this script by hand, or you should know what you're doing"
   exit 1
fi

# params
bigloo_version=$1
hop_version=$2
shift 2

other_pkgs="$@"

apt-get update
apt-get -y --force-yes install $other_pkgs

# compile
cd root
export PATH=$PATH:/usr/local
if [ -z "$MAKE" ]; then
   MAKE=make
fi

# bigloo
# TODO: support bigloo from hg repos? bootstrapping is not fun
if ! [ "$bigloo_version" = "" ]; then
   wget ftp://ftp-sop.inria.fr/indes/fp/Bigloo/bigloo${bigloo_version}.tar.gz
   tar xzvf bigloo${bigloo_version}.tar.gz

   # /root/compile_bigloo_hop.sh: line 33: cd: bigloo3.4b-beta25Oct10: No such file or directory
   bigloo_version=$(echo $bigloo_version | cut -d '-' -f 1)
   ( cd bigloo${bigloo_version}
     set -e
     ./configure && $MAKE && make install
   )
   ldconfig
fi

if ! [ "$hop_version" = "" ]; then
   if ! [ "${hop_version:0:3}" = "hg;" ]; then
      wget ftp://ftp-sop.inria.fr/indes/fp/Hop/hop-${hop_version}.tar.gz
      tar xzvf hop-${hop_version}.tar.gz
   else
      # format: hg:repo:revision
      hg_repo=$(echo $hop_version | cut -d ';' -f 2)
      # default repo
      hg_repo=${hg_repo:-ssh://hop@hop.inria.fr/hop}
      hg_revision=$(echo $hop_version | cut -d ';' -f 3)
      hop_version="hg"
      hg clone $hg_repo hop-hg
      ( cd hop-hg
        hg update -C $hg_revision
      )
   fi

   ( cd hop-${hop_version}
   set -e
   ./configure && $MAKE && make install
   )
   ldconfig
   adduser --system --ingroup users --shell /bin/sh hop
   passwd hop <<EOF
hop
hop
EOF

   mkdir -pv /home/hop/src
   chown -vR hop.users /home/hop

   mkdir -pv /home/hop/.config/hop
   chown -vR hop.users /home/hop/.config/hop

   cat > /home/hop/.config/hop/wizard.hop <<EOF
;; generated file, Hop Wizard Tue May 10 17:57:59 2011
;; anonymous user
(add-user! "anonymous")

;; admin
(add-user! "admin" :groups (quote (admin exec)) :password "+04ffaddbbfe100f27103e9addeeb4886" :directories (cons "/tmp" (hop-path)) :services (quote *))

;; hop
(add-user! "hop" :groups (quote (exec)) :password "+7f492a6526aff9268ca37dbf03f74be6" :directories (cons* "/home/hop" "/tmp" (hop-path)) :services (quote *))
EOF
   chown -vR hop.users /home/hop/.config/hop/wizard.hop

   cat > /home/hop/.config/hop/hoprc.hop <<EOF
;; generated file, Hop Wizard Tue May 10 17:57:59 2011
;; default rc file
(let ((path (make-file-name (hop-etc-directory) "hoprc.hop"))) (when (file-exists? path) (hop-load path)))

;; wizard file
(hop-load-rc "wizard.hop")

;; proxying
(hop-enable-proxying-set! #t)

;; proxy authentication
(hop-proxy-authentication-set! #t)

;; proxy remote connections
(hop-proxy-allow-remote-client-set! #f)

;; proxy IP mask
(hop-proxy-ip-mask-set! "255.255.255.255")

;; WebDAV
(hop-enable-webdav-set! #t)

;; repository
(hop-hz-repositories-add!
  (make-file-path (getenv "HOME") ".config" "hop" "repository"))
EOF
   chown -vR hop.users /home/hop/.config/hop/hoprc.hop

   cat > /etc/sudoers <<EOF
# cat /etc/sudoers  
# /etc/sudoers
#
# This file MUST be edited with the 'visudo' command as root.
#
# See the man page for details on how to write a sudoers file.
#

Defaults        env_reset

# Host alias specification

# User alias specification

# Cmnd alias specification

# User privilege specification
root    ALL=(ALL) ALL
hop     ALL=NOPASSWD: ALL

# Allow members of group sudo to execute any command
# (Note that later entries override this, so you might need to move
# it further down)
%sudo ALL=(ALL) ALL
#
#includedir /etc/sudoers.d
EOF

fi
