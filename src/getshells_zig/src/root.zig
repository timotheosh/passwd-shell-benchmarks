// getshells - core logic
// Reads a unix passwd file and tallies login shells
const std = @import("std");
const Io = std.Io;

const MAX_SHELLS = 64;
const SHELL_LEN = 64;

pub fn run(io: Io) !void {
    const cwd = Io.Dir.cwd();
    const file = try cwd.openFile(io, "passwd", .{});
    defer file.close(io);

    var read_buf: [4096]u8 = undefined;
    var file_reader: Io.File.Reader = .init(file, io, &read_buf);
    const reader = &file_reader.interface;

    const ShellName = [SHELL_LEN]u8;
    var shells = std.mem.zeroes([MAX_SHELLS]ShellName);
    var shell_lengths = std.mem.zeroes([MAX_SHELLS]usize);
    var shellcnt = std.mem.zeroes([MAX_SHELLS]i32);
    var numshells: usize = 0;

    // takeDelimiter consumes the delimiter and returns null at EOF
    while (reader.takeDelimiter('\n') catch null) |line| {
        const last_colon = std.mem.lastIndexOfScalar(u8, line, ':') orelse continue;
        const shell = line[last_colon + 1 ..];
        if (shell.len == 0 or shell.len >= SHELL_LEN) continue;

        var found = false;
        for (0..numshells) |k| {
            if (std.mem.eql(u8, shells[k][0..shell_lengths[k]], shell)) {
                shellcnt[k] += 1;
                found = true;
                break;
            }
        }

        if (!found and numshells < MAX_SHELLS) {
            @memcpy(shells[numshells][0..shell.len], shell);
            shell_lengths[numshells] = shell.len;
            shellcnt[numshells] = 1;
            numshells += 1;
        }
    }

    var stdout_buf: [4096]u8 = undefined;
    var stdout_writer: Io.File.Writer = .init(.stdout(), io, &stdout_buf);
    const out = &stdout_writer.interface;

    for (0..numshells) |i| {
        try out.print("{s:<18}:\t{d}\n", .{ shells[i][0..shell_lengths[i]], shellcnt[i] });
    }
    try out.flush();
}
