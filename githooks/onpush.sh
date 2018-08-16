#!/bin/bash

PROJECT_DIR=$PWD
GITHOOKS_DIR=$PROJECT_DIR/githooks
TMP=$GITHOOKS_DIR/tmp

COMMIT=`git rev-parse --short HEAD`
BRANCH=$1

. $GITHOOKS_DIR/CONFIG.sh

[ -z "$HOST" ] && HOST=`hostname`

for f in $GITHOOKS_DIR/onpush.d/*; do
  set -a
  source $f || exit 1
  set +a
done
