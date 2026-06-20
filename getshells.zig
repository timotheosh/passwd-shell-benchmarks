// getshells.zig 
// demo program to read a unix password file and show 
// the instances of each login shell 
const std = @import("std");

const MAX_SHELLS = 64;
const SHELL_LEN = 64;

const ShellName = [SHELL_LEN]u8;

pub fn main() !void {
    const file = std.fs.cwd().openFile("passwd", .{}) catch |err| {
        std.debug.print("Error opening file: {}\n", .{err});
        return err;
    };
    defer file.close();

    // Create a 4KB backing block directly inside the file stream interface
    var io_buffer: [4096]u8 = undefined;
    var file_reader = file.reader(&io_buffer);
    const in_stream = &file_reader.interface;

    // Data structures for tracking shells
    var shells = std.mem.zeroes([MAX_SHELLS]ShellName);
    var shell_lengths = std.mem.zeroes([MAX_SHELLS]usize);
    var shellcnt = std.mem.zeroes([MAX_SHELLS]i32);
    var numshells: usize = 0;

    // Read file line-by-line via the 0.15 streaming exclusive delimiter block
    while (true) {
        const line = in_stream.takeDelimiterExclusive('\n') catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        // Find the last colon, equivalent to strrchr(line, ':')
        const last_colon_idx = std.mem.lastIndexOfScalar(u8, line, ':') orelse continue;

        // Extract the shell substring safely
        const shell_slice = line[last_colon_idx + 1 ..];
        if (shell_slice.len == 0 or shell_slice.len >= SHELL_LEN) continue;

        var found = false;

        // Search if shell already exists in our table
        for (0..numshells) |k| {
            const active_shell = shells[k][0..shell_lengths[k]];
            if (std.mem.eql(u8, active_shell, shell_slice)) {
                shellcnt[k] += 1;
                found = true;
                break;
            }
        }

        // If it's a new shell, add it safely
        if (!found and numshells < MAX_SHELLS) {
            @memcpy(shells[numshells][0..shell_slice.len], shell_slice);
            shell_lengths[numshells] = shell_slice.len;
            shellcnt[numshells] = 1;
            numshells += 1;
        }
    }

    // Access stdout via std.fs.File and attach a buffer
    var stdout_buf: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buf);
    const stdout = &stdout_writer.interface;

    for (0..numshells) |i| {
        const active_shell = shells[i][0..shell_lengths[i]];
        try stdout.print("{s:<18}:\t{d}\n", .{ active_shell, shellcnt[i] });
    }

    // Explicitly flush to display all printed buffered data
    try stdout.flush();
}

