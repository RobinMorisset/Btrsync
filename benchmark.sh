#!/bin/bash

# benchmark.sh FROM TO
# run btrsync $FROM $TO and rsync --delete -rIv --chmod=* --no-* $FROM $TO and show the data transferred

set -x

FROM=$(echo "$1" | sed 's![^/]$!&/!')
TO=$(echo "$2" | sed 's![^/]$!&/!')
TDIR=`mktemp -d` || exit 1
TDIR2=`mktemp -d` || { rm -Rf $TDIR; exit 1; }

trap "{ rm -Rf $TDIR; rm -Rf $TDIR2; }" EXIT

rsync -a "$TO" "$TDIR/"
rsync -a "$TO" "$TDIR2/"
T1=$(date +%s%N | cut -b1-13)
BTRSYNC=$(time btrsync.sh "$FROM" "$TDIR/" | tail -1)
T2=$(date +%s%N | cut -b1-13)
RSYNC=$(time rsync --delete -rIv --no-perms --chmod="a=rx,u+w" --no-links \
  --no-hard-links --no-acls --no-xattrs --no-owner --no-group --no-devices \
  --no-specials --no-times "$FROM" "$TDIR2/" | tail -2 | head -1)
T3=$(date +%s%N | cut -b1-13)
TBTRSYNC=$(($T2 - $T1))
TRSYNC=$(($T3 - $T2))
diff -r "$TDIR/" "$TDIR2/" || { echo 'ERROR: result mismatch'; exit 1; }

echo -n "$FROM "
echo -n "$TO "
echo "$BTRSYNC" | awk '{printf "%d %d ", $3, $6}'
echo "$RSYNC" | awk '{printf "%d %d ", $2, $5}'
DIFFERENCE=$(echo "$BTRSYNC $RSYNC" | awk '{print $3 + $6 - $9 - $12}')
echo "$DIFFERENCE $TBTRSYNC $TRSYNC"
