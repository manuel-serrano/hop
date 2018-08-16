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

echo "[ " > $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME/$benchmark.json

sepentry=""

for ((s=0; s<$AUTOBENCH_STACK_SHIFT; s++)) do
    tm=
    p=
    times=
    cycles=
    sep="["
    
    mkstring $s

    for ((i=0; i<$AUTOBENCH_ITER; i++)) do
        p=`env -i FILLER=$FILLER $PERF stat $TMP/$benchmark 2> $TMP/perf.run > /dev/null`
        cy=`grep "   cycles  " $TMP/perf.run | awk '{print $1}' | sed 's/,//g'`
        tm=`grep " seconds time elapsed" $TMP/perf.run | awk '{print $1}'`

        times="$times$sep $tm"
        cycles="$cycles$sep $cy"

        sep=","
    done

    echo $sepentry >> \
      $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME/$benchmark.json

    echo "{ stack_shift: $s, times: $times ], cycles: $cycles ] }"
    echo "{ stack_shift: $s, times: $times ], cycles: $cycles ] }" >> \
	 $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME/$benchmark.json
    
    sepentry=","
done

echo "] " >> $GITHOOKS_DIR/autobench/results/$system/$HOSTNAME/$benchmark.json
