#!/usr/bin/env bash

sed -r '/^\\bibliographystyle.*/ {
  r btrsync-tgc.bbl
  d
}
/\\input\{results\.tex\}/ {
  r results.tex
  d
}' < btrsync.tex > btrsync-tgc-single.tex
