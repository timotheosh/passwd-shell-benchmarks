#!/usr/bin/perl

my %h;
open(my $fh, '<', 'passwd') or die "Cannot open passwd: $!";
while (<$fh>) {
    my $i = rindex($_, ':');
    $h{substr($_, $i + 1, -1)}++ if $i >= 0;
}
close $fh;
printf("%-18s:\t%d\n", $_, $h{$_}) for sort keys %h;

