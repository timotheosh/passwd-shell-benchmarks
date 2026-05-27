#!/usr/bin/env crystal

shellcnt = Hash(String, Int32).new(0)

File.each_line("passwd") do |line|
  shell = line.rpartition(':')[2].chomp
  shellcnt[shell] += 1
end

shellcnt.each do |shell, n|
  puts "#{shell.ljust(20)}: #{n}"
end
