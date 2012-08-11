# use this to regenerate the table data

cat results |
  awk '{printf "\\texttt{%s} & \\texttt{%s} & %d & %d & %d & %d & %d & %+.0f \\% & %.1f & %.1f \\\\\n",
    $1, $2, $5, $6, $3, $4, $7, 100*(1.0*($3+$4-5-$6)/($5+$6)), $9/1000, $8/1000}
    END {print "\\hline"}' |sed 's/_/\\_/g;s!test/!!g' | tr -d '/' > results.tex

