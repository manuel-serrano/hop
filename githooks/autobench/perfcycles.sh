#!/bin/bash

[ -z "$PERF" ] && PERF=perf

$PERF stat -x , $* 2>&1 > /dev/null | grep instructions | awk -F, '{ print $1}'
