#/bin/bash

DIR=$1
NUM=$2
TRACE=$3

cat $DIR/ex-$DIR-$NUM.txt | vislcg3 -g $DIR/$DIR.rlx $TRACE
