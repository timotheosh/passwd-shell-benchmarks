#!/usr/bin/crystal run --release

BUFFER_SIZE = 1024 * 1024
MAX_SHELL_LEN = 512

class ShellEntry
  property name : Bytes
  property count : Int32

  def initialize(@name : Bytes, @count : Int32)
  end
end

table = Hash(Int32, Array(ShellEntry)).new

buf = Bytes.new(BUFFER_SIZE)
shell = Bytes.new(MAX_SHELL_LEN)

File.open("passwd") do |file|
  colon_count = 0
  shell_len = 0
  capturing = false

  while (n = file.read(buf)) > 0
    i = 0

    while i < n
      b = buf[i]

      case b
      when ':'.ord.to_u8
        colon_count += 1

        if colon_count == 6
          capturing = true
          shell_len = 0
        end

      when '\n'.ord.to_u8

        if capturing && shell_len > 0

          bucket = table[shell_len] ||= [] of ShellEntry

          found = false

          bucket.each do |entry|
            if entry.name.size == shell_len &&
               entry.name.to_unsafe.memcmp(
                 shell.to_unsafe,
                 shell_len
               ) == 0

              entry.count += 1
              found = true
              break
            end
          end

          unless found
            copy = Bytes.new(shell_len)
            copy.copy_from(shell[0, shell_len])

            bucket << ShellEntry.new(copy, 1)
          end
        end

        colon_count = 0
        shell_len = 0
        capturing = false

      else
        if capturing
          shell[shell_len] = b
          shell_len += 1
        end
      end

      i += 1
    end
  end
end

table.each_value do |bucket|
  bucket.each do |entry|
    puts "#{String.new(entry.name)} : #{entry.count}"
  end
end
