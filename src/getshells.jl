#!/usr/bin/env julia

shellcnt = Dict{String,Int64}()

try
  open("passwd", "r") do f
  while ! eof(f)
    s = readline(f)
    pwline = split(s, ":")
    shell = pwline[7]
    shellcnt["$shell"] = get!(shellcnt, "$shell", 0) +1
  end
end
catch
    println("file not found")
end

for i in keys(shellcnt)
  println(i, ":\t", shellcnt[i])
end


