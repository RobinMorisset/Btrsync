#!/bin/bash

FIFO=`mktemp -t btrsync.fifo.XXXXXX` || exit 1
TFILE=`mktemp -t btrsync.output.XXXXXX` || { rm -f $FIFO; exit 1; }
trap "{ rm -f $TFILE; rm -f $FIFO; }" EXIT

btrsync $* > $TFILE
if [ $? -ne 0 ]
then
    cat $TFILE
    rm $TFILE
    exit 1
fi
{ read cmdNeil; read cmdOscar; } < $TFILE
echo "Neil command:  $cmdNeil"
echo "Oscar command: $cmdOscar"

mkfifo $FIFO
eval $cmdNeil < $FIFO | eval $cmdOscar > $FIFO
