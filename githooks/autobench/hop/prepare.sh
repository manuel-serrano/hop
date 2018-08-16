#!/bin/bash
#*=====================================================================*/
#*    serrano/prgm/project/githooks/hop/autobench/hop/prepare.sh       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Aug 13 16:19:56 2018                          */
#*    Last change :  Thu Aug 16 13:32:24 2018 (serrano)                */
#*    Copyright   :  2018 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    Benchmarks compilation                                           */
#*=====================================================================*/

$PROJECT_DIR/bin/hopc -Ox -o $TMP/$2 $PROJECT_DIR/bench/$2/$2.js

