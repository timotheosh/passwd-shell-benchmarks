#!/bin/sh

set -eu

SRCDIR=src
BINDIR=bin
LOGDIR=logs
MANIFEST=benchmark.list

mkdir -p "$BINDIR" "$LOGDIR"
: > "$MANIFEST"

log() {
	printf '[%s] %s\n' "$1" "$2" >&2
}

register() {
	printf '%s|%s\n' "$1" "$2" >> "$MANIFEST"
}

make_wrapper() {
	name=$1
	runtime=$2
	source=$3

	cat > "${BINDIR}/${name}" <<EOF
#!/bin/sh
DIR=\$(CDPATH= cd -- "\$(dirname -- "\$0")" && pwd)
exec ${runtime} "\$DIR/../src/${source}" "\$@"
EOF

	chmod +x "${BINDIR}/${name}"
}

make_wrapper_args() {
	name=$1
	source=$2
	shift 2

	cat > "${BINDIR}/${name}" <<EOF
#!/bin/sh
DIR=\$(CDPATH= cd -- "\$(dirname -- "\$0")" && pwd)
exec $* "\$DIR/../src/${source}" "\$@"
EOF

	chmod +x "${BINDIR}/${name}"
}

#
# Native binaries
#

if [ -n "${CC:-}" ]; then
	:
elif command -v clang >/dev/null 2>&1; then
	CC=clang
elif command -v gcc >/dev/null 2>&1; then
	CC=gcc
elif command -v cc >/dev/null 2>&1; then
	CC=cc
else
	CC=
fi

if [ -n "$CC" ]; then
	log BUILD C
	"$CC" -O3 \
		"${SRCDIR}/getshells.c" \
		-o "${BINDIR}/getshells-c" \
		>"${LOGDIR}/c.log" 2>&1

	register c "${BINDIR}/getshells-c"
fi

if command -v cargo >/dev/null 2>&1; then
	log BUILD Rust
	(
		cd "${SRCDIR}/getshells_rust"
		cargo build --release --all-features \
			>"../../${LOGDIR}/rust.log" 2>&1
	)

	cp "${SRCDIR}/getshells_rust/target/release/getshells" \
		"${BINDIR}/getshells-rust"

	register rust "${BINDIR}/getshells-rust"
fi

if command -v go >/dev/null 2>&1; then
	log BUILD Go
	go build \
		-o "${BINDIR}/getshells-go" \
		"${SRCDIR}/getshells.go" \
		>"${LOGDIR}/go.log" 2>&1

	register go "${BINDIR}/getshells-go"
fi

if command -v gfortran >/dev/null 2>&1; then
	log BUILD Fortran
	gfortran \
		-o "${BINDIR}/getshells-fortran" \
		"${SRCDIR}/getshells.f90" \
		>"${LOGDIR}/fortran.log" 2>&1

	register fortran "${BINDIR}/getshells-fortran"
fi

if command -v sbcl >/dev/null 2>&1; then
	log BUILD Lisp
	sbcl \
		--dynamic-space-size 1024 \
		--load "${SRCDIR}/getshells.lisp" \
		--eval '(sb-ext:save-lisp-and-die "bin/getshells-lisp" :toplevel #'\''main :executable t :compression nil)' \
		--quit \
		>"${LOGDIR}/lisp.log" 2>&1

	register lisp "${BINDIR}/getshells-lisp"
fi

if command -v crystal >/dev/null 2>&1; then
	log BUILD Crystal
	crystal build \
		--release --no-debug \
		-s -t -p --verbose --no-color \
		-o "${BINDIR}/getshells-cr" \
		"${SRCDIR}/getshells.cr" \
		>"${LOGDIR}/crystal.log" 2>&1

	register crystal "${BINDIR}/getshells-cr"
fi

if command -v ghc >/dev/null 2>&1; then
	log BUILD Haskell
	ghc \
		-o "${BINDIR}/getshells-hs" \
		"${SRCDIR}/getshells.hs" \
		-no-keep-hi-files \
		-no-keep-o-files \
		>"${LOGDIR}/haskell.log" 2>&1

	register haskell "${BINDIR}/getshells-hs"
fi

#
# Script wrappers
#

if command -v python3 >/dev/null 2>&1; then
	log WRAP Python
	make_wrapper getshells-py python3 getshells.py
	register python "${BINDIR}/getshells-py"
fi

if command -v perl >/dev/null 2>&1; then
	log WRAP Perl
	make_wrapper getshells-pl perl getshells.pl
	register perl "${BINDIR}/getshells-pl"

	log WRAP Perl-One-Liner
	make_wrapper getshells-one-liner-pl sh getshells-one-liner.pl
	register perl-one-liner "${BINDIR}/getshells-one-liner-pl"
fi

if command -v ruby >/dev/null 2>&1; then
	log WRAP Ruby
	make_wrapper getshells-rb ruby getshells.rb
	register ruby "${BINDIR}/getshells-rb"
fi

if command -v php >/dev/null 2>&1; then
	log WRAP PHP
	make_wrapper getshells-php php getshells.php
	register php "${BINDIR}/getshells-php"
fi

if command -v node >/dev/null 2>&1; then
	log WRAP NodeJS
	make_wrapper getshells-js node getshells.js
	register nodejs "${BINDIR}/getshells-js"
fi

if command -v julia >/dev/null 2>&1; then
	log WRAP Julia
	make_wrapper getshells-jl julia getshells.jl
	register julia "${BINDIR}/getshells-jl"
fi

if command -v lua >/dev/null 2>&1; then
	log WRAP Lua
	make_wrapper getshells-lua lua getshells.lua
	register lua "${BINDIR}/getshells-lua"
fi

if command -v luajit >/dev/null 2>&1; then
	log WRAP LuaJIT
	make_wrapper getshells-luajit luajit getshells.lua
	register luajit "${BINDIR}/getshells-luajit"
fi

if command -v awk >/dev/null 2>&1; then
	log WRAP Awk
	make_wrapper getshells-awk sh getshells.awk
	register awk "${BINDIR}/getshells-awk"
fi

if command -v pwsh >/dev/null 2>&1; then
	log WRAP PowerShell
	make_wrapper getshells-ps1 pwsh getshells.ps1
	register powershell "${BINDIR}/getshells-ps1"
fi

if command -v elixir >/dev/null 2>&1; then
	log WRAP Elixir
	make_wrapper getshells-exs elixir getshells.exs
	register elixir "${BINDIR}/getshells-exs"
fi

if command -v clojure >/dev/null 2>&1; then
	log WRAP Clojure
	make_wrapper getshells-clj clojure getshells.clj
	register clojure "${BINDIR}/getshells-clj"
fi

if command -v bb >/dev/null 2>&1; then
	log WRAP Babashka
	make_wrapper getshells-bb bb getshells-bb.clj
	register babashka "${BINDIR}/getshells-bb"
fi

if command -v guile >/dev/null 2>&1; then
	log WRAP Guile
	make_wrapper_args getshells-guile getshells-guile.scm guile -s
	register guile "${BINDIR}/getshells-guile"

	log WRAP Scheme
	make_wrapper_args getshells-scheme getshells.scm guile -s
	register scheme "${BINDIR}/getshells-scheme"
fi

if command -v raku >/dev/null 2>&1; then
	log WRAP Raku
	make_wrapper getshells-raku raku getshells.raku
	register raku "${BINDIR}/getshells-raku"
fi
