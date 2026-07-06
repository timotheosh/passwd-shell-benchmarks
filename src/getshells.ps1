#!/usr/bin/env pwsh
[System.IO.File]::ReadLines("passwd") | 
    ForEach-Object { $_.Split(':')[6] } | 
    Group-Object -NoElement | 
    ForEach-Object { "{0} : {1}" -f $_.Name, $_.Count }

