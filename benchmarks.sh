#!/bin/bash

cat testcases |
  while read a;
    do
      F=$(cut -d' ' -f1<<<$a);
      T=$(cut -d' ' -f2<<<$a);
      benchmark.sh "$F" "$T";
    done | tee results

