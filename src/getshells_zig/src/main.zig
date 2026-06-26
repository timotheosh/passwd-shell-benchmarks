const std = @import("std");
const getshells = @import("getshells");

pub fn main() !void {
    try getshells.run();
}
