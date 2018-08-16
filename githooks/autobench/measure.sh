#!/bin/bash

system=$1
benchmark=$2

mkdir -p $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME

times=
cycles=
sep="["

#* TIMEFORMAT="%3R"                                                    */
#* export TIMEFORMAT                                                   */

for ((i=0; i<$AUTOBENCH_ITER; i++)) do
  tm=`/usr/bin/time -f '%e' $TMP/$benchmark 2>&1 > /dev/null`
  cy=`$GITHOOKS_DIR/autobench/perfcycles.sh $TMP/$benchmark 2>&1`

  times="$times$sep $tm"
  cycles="$cycles$sep $cy"

  sep=","
done

echo "{ times: $times ], cycles: $cycles ] }" \
  | tee $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME/$benchmark.json

