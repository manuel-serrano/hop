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

   mkdir -pv /home/hop/src
   chown -v hop.users /home/hop/src
fi
