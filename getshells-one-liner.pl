perl -ne '$i=rindex($_,":");$h{substr($_,$i+1,-1)}++if$i>=0;END{printf"%-18s:\t%d\n",$_,$h{$_}for sort keys%h}' passwd
