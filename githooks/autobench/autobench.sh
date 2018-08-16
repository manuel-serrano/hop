#!/bin/bash

[ -z "$AUTOBENCH_ITER" ] && AUTOBENCH_ITER=3

if [ -f $GITHOOKS_DIR/CONFIG.autobench.sh ]; then
  . $GITHHOOKS_DIR/CONFIG.autobench.sh
fi  

for h in $HOSTS; do 
  if [ "$h " = "$HOST " ]; then
    for s in $SYSTEMS; do
      if [ -d $GITHOOKS_DIR/autobench/$s ]; then
        echo "system $s"
        set -a
        source $GITHOOKS_DIR/autobench/install.sh $s || exit 1
	for b in $BENCHMARKS; do
          source $GITHOOKS_DIR/autobench/prepare.sh $s $b
          source $GITHOOKS_DIR/autobench/measure.sh $s $b
	done
        source $GITHOOKS_DIR/autobench/commit.sh $s
        source $GITHOOKS_DIR/autobench/cleanup.sh $s
        set +a
        echo "system $s...done"
      fi
    done
  fi  
done
