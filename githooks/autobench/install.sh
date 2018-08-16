#!/bin/bash

mkdir -p $TMP

if [ -f $GITHOOKS_DIR/autobench/$1/install.sh ]; then
  source $GITHOOKS_DIR/autobench/$1/install.sh
fi  
