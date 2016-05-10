#/bin/bash

DIR=$1
TRACE=$2

cat $DIR/ex-$DIR.txt | vislcg3 -g $DIR/$DIR.rlx $TRACE
