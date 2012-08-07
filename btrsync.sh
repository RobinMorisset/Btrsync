#!/bin/bash

FIFO=`mktemp -t btrsync.fifo.XXXXXX` || exit 1
TFILE=`mktemp -t btrsync.output.XXXXXX` || { rm -f $FIFO; exit 1; }
trap "{ rm -f $TFILE; rm -f $FIFO; }" EXIT

btrsync.py "$@" > $TFILE
if [ $? -ne 0 ]
then
  cat $TFILE
  rm $TFILE
  exit 1
fi
{ read ok; read cmdNeil; read cmdOscar; } < $TFILE

if [ "$ok" != "ok" ]
then
  cat $TFILE
  exit 0
fi

echo "Neil command:  $cmdNeil"
echo "Oscar command: $cmdOscar"

rm -f $FIFO
mkfifo $FIFO
eval $cmdOscar < $FIFO | eval $cmdNeil > $FIFO
