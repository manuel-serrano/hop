#!/bin/bash

[ -z "$AUTOBENCH_ITER" ] && AUTOBENCH_ITER=3
[ -z "$AUTOBENCH_STACK_SHIFT" ] && AUTOBENCH_STACK_SHIFT=64

if [ -f $GITHOOKS_DIR/CONFIG.autobench.sh ]; then
  . $GITHHOOKS_DIR/CONFIG.autobench.sh
fi  

for h in $HOSTS; do 
  if [ "$h " = "$HOST " ]; then
    for sys in $SYSTEMS; do
      if [ -d $GITHOOKS_DIR/autobench/$sys ]; then
        echo "system $s"
        set -a
        source $GITHOOKS_DIR/autobench/install.sh $sys || exit 1
	for b in $BENCHMARKS; do
          source $GITHOOKS_DIR/autobench/prepare.sh $sys $b
          source $GITHOOKS_DIR/autobench/measure.sh $sys $b
	done
        source $GITHOOKS_DIR/autobench/commit.sh $sys
        source $GITHOOKS_DIR/autobench/cleanup.sh $sys
        set +a
        echo "system $s...done"
      fi
    done
  fi  
done
