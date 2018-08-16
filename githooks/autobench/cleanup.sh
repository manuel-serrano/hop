#!/bin/bash

if [ -f $GITHOOKS_DIR/autobench/$1/cleanup.sh ]; then
  source $GITHOOKS_DIR/autobench/$1/cleanup.sh
fi  

rm -rf $TMP
