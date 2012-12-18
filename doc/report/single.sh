#!/usr/bin/env bash

sed -r '/^\\bibliographystyle.*/ {
  r btrsync.bbl
  d
}
/\\input\{results\.tex\}/ {
  r results.tex
  d
}' < btrsync.tex > btrsync-single.tex
