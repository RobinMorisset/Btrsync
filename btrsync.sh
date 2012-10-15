#!/bin/bash

date +"%s %N" >&2

FIFO=`mktemp -t btrsync.fifo.XXXXXX` || exit 1
TFILE=`mktemp -t btrsync.output.XXXXXX` || { rm -f $FIFO; exit 1; }
SFILE=`mktemp -t btrsync.status.XXXXXX` || { rm -f $FIFO; rm -f $TFILE; exit 1; }
date +"%s %N" >&2
trap "{ rm -f $TFILE; rm -f $SFILE; rm -f $FIFO; }" EXIT
date +"%s %N" >&2

btrsync_cmd.py --status=$SFILE "$@" > $TFILE
if [ $? -ne 0 ]
then
  cat $TFILE
  rm $TFILE
  exit 1
fi
{ read ok; read cmdNeil; read cmdOscar; } < $TFILE
date +"%s %N" >&2

if [ "$ok" != "ok" ]
then
  cat $TFILE
  exit 0
fi
date +"%s %N" >&2

echo "Neil command:  $cmdNeil"
echo "Oscar command: $cmdOscar"

rm -f $FIFO
mkfifo $FIFO
date +"%s %N" >&2
eval $cmdOscar < $FIFO | eval $cmdNeil > $FIFO
date +"%s %N" >&2
cat $SFILE
