#!/usr/bin/env ruby

shellcnt = Hash.new(0) # use Hash.new to create a new hash with a default value of 0 for any missing keys

if File.exist?("passwd")
  File.foreach("passwd") do |line|
    line.chomp!
    gecos = line.split(':')
    shell = gecos.last
    shellcnt[shell] += 1 # use the += operator to increment the value of each shell instead of using an if/else statement
  end

  shellcnt.each do |key, value|
    printf("%-20s%-8s%-d\n", key, ":", value)
  end
end
