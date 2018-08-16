#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/githooks/hop/autobench/hop/install.sh       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Aug 13 16:09:31 2018                          */
#*    Last change :  Thu Aug 16 13:31:57 2018 (serrano)                */
#*    Copyright   :  2018 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    Hop install file                                                 */
#*=====================================================================*/

[ -z "$CC" ] && CC=gcc
[ -z "$CFLAGS" ] && CFLAGS=-O3

if [ ! -f $GITHOOKS_DIR/autobench/bootstrap/hop ]; then
  mkdir -p $GITHOOKS_DIR/autobench/bootstrap/hop
  mkdir -p $GITHOOKS_DIR/autobench/download
  
  # download a install a first Bigloo version used to bootstrap the github repo
  if [ ! -f $GITHOOKS_DIR/autobench/download/bigloo-latest.tar.gz ]; then
    wget ftp://ftp-sop.inria.fr/indes/fp/Bigloo/bigloo-latest.tar.gz -O $GITHOOKS_DIR/autobench/download/bigloo-latest.tar.gz
  fi

  # install that version
  (cd $GITHOOKS_DIR/autobench/bootstrap/hop; \
   echo "Untaring bigloo..."; \
   tar xfz ../../download/bigloo-latest.tar.gz; \
   cd bigloo4.3b; \
   ./configure --prefix=$GITHOOKS_DIR/autobench/bootstrap/hop --disable-avahi --disable-alsa --disable-gstreamer --disable-pulseaudio --disable-mpg123 --disable-flac --disable-multimedia --disable-wav --jvm=no; \
   echo "Compiling bootstrap Bigloo $(PWD)"; \
   make -j; \
   make install)
fi

# install the current Bigloo version
if [ ! -z "`git diff HEAD HEAD^ ./configure`" -o ! -f config.log -o ! -d $GITHOOKS_DIR/autobench/local ]; then
  echo "configuring hop..."
  ./configure --prefix=$PWD --bigloo=$GITHOOKS_DIR/autobench/bootstrap/hop/bin/bigloo --disable-docs || exit 1
  echo "compiling and testing hop... "
  make -j CONFIGUREOPTS="--jvm=no" || exit 1
  make test && make install || exit 1
else  
  echo "compiling hop... "
  make -j && make install || exit 1
fi

