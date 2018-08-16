#!/bin/bash

system=$1
benchmark=$2

[ -z "$PERF" ] && PERF=perf

mkdir -p $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME

string_res=

#* TIMEFORMAT="%3R"                                                    */
#* export TIMEFORMAT                                                   */

function mkstring() {
  string_res=""

  for ((i=0; i<$1; i++)) do
      string_res+="########"
  done
}
  
export FILLER=""

echo "$2:"

rm -f $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME/$benchmark.json
touch $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME/$benchmark.json

for ((shift=0; shift<$AUTOBENCH_STACK_SHIFT; shift++)) do
    times=
    cycles=
    sep="["
    
    mkstring( shift )
    FILLER=$string_res
    
    env
    
    for ((i=0; i<$AUTOBENCH_ITER; i++)) do
        p=`$PERF stat $TMP/$benchmark 2> $TMP/perf.run > /dev/null`
        cy=`grep "   cycles  " $TMP/perf.run | awk '{print $1}' | sed 's/,//g'`
        tm=`grep " seconds time elapsed" $TMP/perf.run | awk '{print $1}'`

        times="$times$sep $tm"
        cycles="$cycles$sep $cy"

        sep=","
    done
	
    echo "{ shift: $shift, times: $times ], cycles: $cycles ] }"
    echo "{ shift: $shift, times: $times ], cycles: $cycles ] }" >> \
      $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME/$benchmark.json
done


