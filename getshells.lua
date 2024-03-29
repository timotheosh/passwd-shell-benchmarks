#!/usr/bin/env lua
local m = {}
for line in io.lines("passwd") do
	local _, e = string.find(line, ".*:")
	local shell = string.sub(line, e + 1)
	m[shell] = (m[shell] or 0) + 1
end
for key, value in pairs(m) do
	print(key .. ": " .. value)
end
