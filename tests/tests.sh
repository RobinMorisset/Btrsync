#!/bin/bash

TDIR=`mktemp -d -t btrsync.tests.XXXXXX` || exit 1

fail (){
  printf "\033[1;31m[ FAIL %02d ]\033[0m" $1
}

success (){
  printf "\033[1;32m[ SUCCESS ]\033[0m"
}

echo
echo "Results in $TDIR"
echo

#
# Algorithmic tests on absolute path on the same computer
# Compare the result with rsync
# Use all the tests in this folder
#
echo "Algorithmic tests"
TESTS=`ls -d */`
for t2 in $TESTS
do
  t=`basename $t2`
  printf "\n  %-40s" $t
  tt="$TDIR/$t"
  mkdir "$tt"
  cp -R "$t/n" "$tt/n_btrsync"
  cp -R "$t/o" "$tt/o_btrsync"

  START_TIME=$SECONDS
  ../btrsync.sh "$tt/n_btrsync" "$tt/o_btrsync" > "$tt/btrsync.stdout" 2> "$tt/btrsync.stderr"
  END_TIME=$SECONDS
  if [ $? -ne 0 ]; then fail 1; continue; fi

  diff "$t/n" "$tt/n_btrsync" > "$tt/diff_n_n_btrsync"
  if [ $? -ne 0 ]; then fail 2; continue; fi
  diff "$t/n" "$tt/o_btrsync" > "$tt/diff_n_o_btrsync"
  if [ $? -ne 0 ]; then fail 3; continue; fi
  success
  printf "  %3ds" $(($END_TIME-$START_TIME))
done

echo

#
# Other tests (relative path, ...)
#
