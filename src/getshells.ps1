#!/usr/bin/env pwsh 

$shells = @{}

foreach($line in $(get-content passwd)){
   $shell = $line.split(':')[6]
   if($shells.ContainsKey($shell)) {
     $shells.Set_Item($shell, $shells.$shell+1) 
  } else {
    $shells.Add($shell, 1)
  }
 }

foreach($sh in $shells) {
	echo $sh, $sh.shell;
}

