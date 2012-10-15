#!/usr/bin/env bash

sed '/^\\bibliographystyle.*/ {
  r btrsync.bbl
  d
}' < btrsync.tex > btrsync-single.tex
