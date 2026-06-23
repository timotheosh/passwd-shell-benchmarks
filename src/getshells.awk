awk -F: '{ print $7 }' < passwd|sort|uniq -c|sort -nrk1|awk '{ printf "%-18s:\t%d\n", $2, $1 }' 
