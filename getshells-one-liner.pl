perl -F: -ane 'chomp $F[-1]; $h{$F[-1]}++ if $F[-1] ; END { printf "%-18s:\t%d\n", $_, $h{$_} for sort keys %h }' passwd
