#!/bin/bash

# benchmark.sh FROM TO
# run btrsync $FROM $TO and rsync --delete -av $FROM $TO and show the data transferred

set -x

FROM="$1"
TO="$2"
TDIR=`mktemp -d` || exit 1
TDIR2=`mktemp -d` || exit 1

trap "{ rm -Rf $TDIR; rm -Rf $TDIR2; }" EXIT

rsync -a "$TO" "$TDIR/"
rsync -a "$TO" "$TDIR2/"
# compare btrsync and rsync invocations
BTRSYNC=$(time ./btrsync.sh "$FROM" "$TDIR/" | tail -1)
RSYNC=$(time rsync --delete -av "$FROM" "$TDIR2/" | tail -2 | head -1)
diff -r "$TDIR/" "$TDIR2/" || { echo 'ERROR: result mismatch'; exit 1; }

echo -n "$FROM "
echo -n "$TO "
echo "$BTRSYNC" | awk '{printf "%d %d ", $3, $6}'
echo "$RSYNC" | awk '{printf "%d %d ", $2, $5}'
DIFFERENCE=$(echo "$BTRSYNC $RSYNC" | awk '{print $3 + $6 - $9 - $12}')
echo $DIFFERENCE
