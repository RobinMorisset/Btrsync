#!/bin/bash

TDIR=`mktemp -d -t btrsync.tests.XXXXXX` || exit 1
DIR=`pwd`

fail (){
  printf "\033[1;31m[ FAIL %02d ]\033[0m" $1
}

success (){
  printf "\033[1;32m[ SUCCESS ]\033[0m"
}

# Print a list of all files in the current folder together with their permission
# Output:
#   ./a.txt 644
#   ./a.out 755
print_perm (){
  find . -exec stat -c "%n %a" {} \; | grep -v -E '^\. ...$' | sort 
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
for t2 in *
do
  if [ ! -d "$t2" ]
  then
    continue
  fi
  if [[ "$t2" == *-disable ]]
  then
    continue
  fi

  t=`basename "$t2"`
  printf "\n  %-40s" "$t"
  tt="$TDIR/$t"
  mkdir "$tt"
  cp -R "$t/n" "$tt/n_btrsync"
  cp -R "$t/o" "$tt/o_btrsync"

  START_TIME=$SECONDS
  ../btrsync.sh "$tt/n_btrsync" "$tt/o_btrsync" > "$tt/btrsync.stdout" 2> "$tt/btrsync.stderr"
  END_TIME=$SECONDS
  if [ $? -ne 0 ]; then fail 1; continue; fi

  # Check files are the same in the original Neil folder and in the folders synchronized by btrsync
  diff -r "$t/n" "$tt/n_btrsync" > "$tt/diff_n_n_btrsync"
  if [ $? -ne 0 ]; then fail 2; continue; fi
  diff -r "$t/n" "$tt/o_btrsync" > "$tt/diff_n_o_btrsync"
  if [ $? -ne 0 ]; then fail 3; continue; fi

  cd "$tt/n_btrsync" && print_perm > "$tt/perm_n_btrsync"
  cd $DIR
  cd "$tt/o_btrsync" && print_perm > "$tt/perm_o_btrsync"
  cd $DIR
  cd "$t/n" && print_perm > "$tt/perm_n"
  cd $DIR

  # Idem for permissions
  diff "$tt/perm_n" "$tt/perm_n_btrsync" > "$tt/diff_perm_n_n_btrsync"
  if [ $? -ne 0 ]; then fail 4; continue; fi
  diff "$tt/perm_n" "$tt/perm_o_btrsync" > "$tt/diff_perm_n_o_btrsync"
  if [ $? -ne 0 ]; then fail 5; continue; fi

  success
  printf "  %3ds" $(($END_TIME-$START_TIME))
done

echo

#
# Other tests (relative path, ...)
#
