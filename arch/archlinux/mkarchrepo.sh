#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/distrib/mkarchrepo.sh                               */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed May 23 13:52:02 2012                          */
#*    Last change :  Sun Jun  9 17:33:44 2013 (serrano)                */
#*    Copyright   :  2012-13 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Build a ARCHLINUX repository for Bigloo and Hop.                 */
#*    -------------------------------------------------------------    */
#*    To avoid downloading source file run as:                         */
#*       % mkarchrepo.sh -e                                            */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    default configuration                                            */
#*---------------------------------------------------------------------*/
bigloo=yes
hop=yes

bigloo_version=4.0b

hop_version=2.4.1
hop_release=

target=/tmp/ARCHLINUX/`uname -m`
repo=$HOME/prgm/distrib
tmp=`mktemp -d`
extract=yes

makepkgopts=--nocheck 

#*---------------------------------------------------------------------*/
#*    argument parsing                                                 */
#*---------------------------------------------------------------------*/
compile=true

while : ; do
  case $1 in
    "")
      break;;

    --repo=*)
      repo="`echo $1 | sed 's/^[^=]*=//'`";;

    --localrepo)
      bigloo_repo=scp://localhost:22$repo;
      hop_repo=scp://localhost:22$repo;;

    --target=*)
      target="`echo $1 | sed 's/^[^=]*=//'`";;

    --compile)
      compile=true;;

    --no-compile)
      compile=false;;

    --tmp=*)
      tmp="`echo $1 | sed 's/^[^=]*=//'`";;

    -e|--noextract)
      makepkgopts="-e $makepkgopts";
      extract=no;;

    --no-bigloo)
      bigloo=no;;

    --no-hop)
      hop=no;;

    -h|--help)
      echo "Usage: $0 [options]" >&2;
      echo "" >&2;
      echo "  --target=dir [default $target]" >&2;
      echo "  --repo=dir [default $repo]" >&2;
      echo "  -e|--noextract [skip download]" >&2;
      echo "  --tmp=dir [default $tmp]" >&2;
      echo "  --compile" >&2;
      echo "  --no-compile" >&2;
      echo "  --no-bigloo" >&2;
      echo "  --no-hop" >&2;
      exit 0;;

    *)
     echo "*** ERROR: Illegal option \"$1\", see $0 --help";;
  esac
  shift
done

#*---------------------------------------------------------------------*/
#*    mkarchpkg ...                                                    */
#*---------------------------------------------------------------------*/
mkarchpkg() {
  base=$1
  dash=$2
  version=$3
  release=$4
  bglversion=$5

  if [ "$release " != " " ]; then
    pkg=$base$dash$version$dash$release
  else
    pkg=$base$dash$version
  fi

  tarball=$pkg.tar.gz
    
  # copy the tarbar file
  if [ -f $repo/$tarball ]; then
    cp $repo/$tarball $tmp;
  else 
    wget $repo/$tarball -O $tmp/$tarball;
  fi
 
  if [ ! -f $tmp/$tarball ]; then
    echo "*** ERROR: cannot find $tmp/$tarball file"
    exit 1;
  fi

  (cd $tmp && 
   tar xf $tarball $pkg/arch/archlinux &&
   md5sum=`md5sum $tarball | awk '{print $1}'` &&

   if [ "$extract " = "no " ]; then
     mkdir -p $pkg/arch/archlinux/src
     tar xf $repo/$tarball -C $pkg/arch/archlinux/src 

     if [ ! "$release " = " " ]; then
        mv $pkg/arch/archlinux/src/$pkg $pkg/arch/archlinux/src/$base$dash$version$release
     fi
   fi
     
   cd $pkg/arch/archlinux &&
   cat PKGBUILD.in \
     | sed -e "s|@MD5SUM@|$md5sum|" \
           -e "s|@VERSION@|$version$release|" \
           -e "s|@BGLVERSION@|$bglversion|" \
     > PKGBUILD

   makepkg $makepkgopts || exit 1
   for p in *.pkg.tar.xz; do \
     cp $p $target && sudo pacman -U $p;
   done)
}

#*---------------------------------------------------------------------*/
#*    Build the Bigloo archlinux packages                              */
#*---------------------------------------------------------------------*/
if [ "$compile " = "true " ]; then
  mkdir -p $target
  mkdir -p $tmp
  if [ $bigloo = "yes" ]; then
    mkarchpkg bigloo "" $bigloo_version "" ""
  fi
  if [ $hop = "yes" ]; then
    mkarchpkg hop - $hop_version "$hop_release" $bigloo_version
  fi
fi

# generate the database
(cd $target && repo-add hop.db.tar.gz *.xz)

# remove the tmp directory
/bin/rm -rf $tmp



 
 
