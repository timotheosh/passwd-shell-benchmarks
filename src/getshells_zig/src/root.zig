// getshells - core logic
// Reads a unix passwd file and tallies login shells
// Uses std.posix / std.os.linux for cross-version compatibility
// (tested on Zig 0.15.x and 0.17-dev)
const std = @import("std");
const posix = std.posix;
const linux = std.os.linux;

const MAX_SHELLS = 64;
const SHELL_LEN = 64;

const ShellName = [SHELL_LEN]u8;

fn processLine(
    line: []const u8,
    shells: *[MAX_SHELLS]ShellName,
    shell_lengths: *[MAX_SHELLS]usize,
    shellcnt: *[MAX_SHELLS]i32,
    numshells: *usize,
) void {
    const last_colon = std.mem.lastIndexOfScalar(u8, line, ':') orelse return;
    const shell = line[last_colon + 1 ..];
    if (shell.len == 0 or shell.len >= SHELL_LEN) return;

    for (0..numshells.*) |k| {
        if (std.mem.eql(u8, shells[k][0..shell_lengths[k]], shell)) {
            shellcnt[k] += 1;
            return;
        }
    }

    if (numshells.* < MAX_SHELLS) {
        @memcpy(shells[numshells.*][0..shell.len], shell);
        shell_lengths[numshells.*] = shell.len;
        shellcnt[numshells.*] = 1;
        numshells.* += 1;
    }
}

pub fn run() !void {
    const fd = try posix.openat(posix.AT.FDCWD, "passwd", .{ .ACCMODE = .RDONLY }, 0);
    defer _ = linux.close(fd);

    var shells = std.mem.zeroes([MAX_SHELLS]ShellName);
    var shell_lengths = std.mem.zeroes([MAX_SHELLS]usize);
    var shellcnt = std.mem.zeroes([MAX_SHELLS]i32);
    var numshells: usize = 0;

    var read_buf: [8192]u8 = undefined;
    var line_buf: [256]u8 = undefined;
    var line_len: usize = 0;

    while (true) {
        const n = try posix.read(fd, &read_buf);
        if (n == 0) break;
        for (read_buf[0..n]) |c| {
            if (c == '\n') {
                processLine(line_buf[0..line_len], &shells, &shell_lengths, &shellcnt, &numshells);
                line_len = 0;
            } else if (line_len < line_buf.len) {
                line_buf[line_len] = c;
                line_len += 1;
            }
        }
    }
    // Handle last line if file doesn't end with newline
    if (line_len > 0) {
        processLine(line_buf[0..line_len], &shells, &shell_lengths, &shellcnt, &numshells);
    }

    var out_buf: [4096]u8 = undefined;
    var out_pos: usize = 0;

    for (0..numshells) |i| {
        const shell_str = shells[i][0..shell_lengths[i]];
        const line = std.fmt.bufPrint(out_buf[out_pos..], "{s:<18}:\t{d}\n", .{ shell_str, shellcnt[i] }) catch break;
        out_pos += line.len;
    }

    var written: usize = 0;
    while (written < out_pos) {
        const rc = linux.write(posix.STDOUT_FILENO, out_buf[written..out_pos].ptr, out_pos - written);
        const n = @as(isize, @bitCast(rc));
        if (n < 0) return error.WriteError;
        written += @intCast(n);
    }
}
