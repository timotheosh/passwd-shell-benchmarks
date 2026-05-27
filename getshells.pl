#!/usr/bin/perl

use strict;
use warnings;

my %pwhash;

open(my $fh, '<', 'passwd') or die "Cannot open passwd: $!";

while (<$fh>) {
    chomp;
    my $shell = (split /:/)[-1];
    $pwhash{$shell}++ if $shell;
}
close($fh);

foreach my $shell (sort keys %pwhash) {
    printf("%-18s:\t%d\n", $shell, $pwhash{$shell});
}

