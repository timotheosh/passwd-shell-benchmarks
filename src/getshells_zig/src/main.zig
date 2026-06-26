const std = @import("std");
const getshells = @import("getshells");

pub fn main(init: std.process.Init) !void {
    try getshells.run(init.io);
}
