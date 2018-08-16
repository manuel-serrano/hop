#!/bin/bash

system=$1
benchmark=$2

[ -z "$PERF" ] && PERF=perf

mkdir -p $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME

#* TIMEFORMAT="%3R"                                                    */
#* export TIMEFORMAT                                                   */

function mkstring() {
  FILLER=""

  for ((i=0; i<$1; i++)) do
      FILLER+="########"
  done
}
  
export FILLER=""

echo "$2:"

rm -f $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME/$benchmark.json
touch $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME/$benchmark.json

for ((s=0; s<$AUTOBENCH_STACK_SHIFT; s++)) do
    tm=
    p=
    times=
    cycles=
    sep="["
    
    mkstring $s

    export -n tm
    export -n p
    export -n times
    export -n cycles
    export -n sep
    
    for ((i=0; i<$AUTOBENCH_ITER; i++)) do
        p=`$PERF stat $TMP/$benchmark 2> $TMP/perf.run > /dev/null`
        cy=`grep "   cycles  " $TMP/perf.run | awk '{print $1}' | sed 's/,//g'`
        tm=`grep " seconds time elapsed" $TMP/perf.run | awk '{print $1}'`

        times="$times$sep $tm"
        cycles="$cycles$sep $cy"

        sep=","
    done
	
    echo "{ stack_shift: $s, times: $times ], cycles: $cycles ] }"
    echo "{ stack_shift: $s, times: $times ], cycles: $cycles ] }" >> \
      $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME/$benchmark.json
done


