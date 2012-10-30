#!/usr/bin/perl
# use SI prefixes in results.tex

sub prefix {
  $a = $_[0];
  return sprintf("%.0fM", $a/(1000*1000)) if abs($a) > 1000*1000;
  return sprintf("%.0fk", $a/(1000)) if abs($a) > 1000;
  return $a;
}

while (<>) {
  my @l = split;
  for (my $i = 5; $i < 15; $i+=2) {
    $l[$i] = prefix($l[$i]);
  }
  my $x = join(" ", @l);
  print "$x\n";
}

