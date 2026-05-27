#!/usr/bin/raku

my $file = 'passwd';
die "Cannot open passwd: File not found" unless $file.IO.e;

my %pwhash = $file.IO.lines.map({ .split(':').tail }).grep({ .so }).Bag;

for %pwhash.keys.sort -> $shell {
    printf "%-18s:\t%d\n", $shell, %pwhash{$shell};
}

