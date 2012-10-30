# use this to regenerate the table data

cat results |
  awk '{printf "\\texttt{%s} & \\texttt{%s} & & %d & %d & %d & %d & %d & %.1f & & %.1f & %.1f \\\\\n",
  $1, $2, $5, $6, $3, $4, $7, ($3+$4)/($5+$6), $9/1000, $8/1000}' | ./si.pl |
    sed 's/_/\\_/g;s!test/!!g' | tr -d '/' > results.tex

