#!/usr/bin/crystal run --release

class ShellCounter
  getter counts = Hash(String, Int32).new(0)

  def add(line : String)
    i = line.rindex(':') || return
    @counts[line.byte_slice(i + 1)] += 1
  end
end

sc = ShellCounter.new
File.each_line("passwd") { |line| sc.add(line) }

sc.counts.each { |shell, n| puts("#{shell.ljust(20)}: #{n}") }
