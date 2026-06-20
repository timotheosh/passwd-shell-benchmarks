use std::{
    fs,
    io::{stdout, Write},
};

use ahash::AHashMap;

fn main() {
    let data = fs::read("passwd").expect("failed to read passwd");
    let mut hs: AHashMap<&[u8], u32> = AHashMap::with_capacity(512);

    for line in data.split(|&b| b == b'\n') {
        let line = line.strip_suffix(b"\r").unwrap_or(line);
        if line.is_empty() {
            continue;
        }
        let shell = match memchr::memrchr(b':', line) {
            Some(pos) => &line[pos + 1..],
            None => line,
        };
        *hs.entry(shell).or_insert(0) += 1;
    }

    let mut stdout = stdout().lock();
    for (shell, count) in &hs {
        let _ = stdout.write_all(shell);
        let _ = writeln!(&mut stdout, ": {count}");
    }
}
