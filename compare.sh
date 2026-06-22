#!/bin/sh

# Check for C Compiler
if [ -n "$CC" ]; then
	:
elif command -v clang >/dev/null 2>&1; then
	CC=clang
elif command -v gcc >/dev/null 2>&1; then
	CC=gcc
elif command -v cc >/dev/null 2>&1; then
	CC=cc
fi

if [ -n "${CC}" ]; then
	CPROG=getshells-c
	${CC} -O3 getshells.c -o ${CPROG}
else
	echo "C Compiler not found."
fi

# Check for rust compiler
if command -v cargo >/dev/null 2>&1; then
	RSPROG="getshells-rust"
	cd "getshells_rust" || echo "getshells_rust folder not found"
	cargo build --release --all-features
	cd ".."
 	cp getshells_rust/target/release/getshells ${RSPROG}
else
	echo "cargo was not found"
fi

# Check for golang compiler
if command -v go >/dev/null 2>&1; then
	GOPROG=getshells-go
	go build getshells.go
	mv getshells ${GOPROG}
else
	echo "Golang compiler not found."
fi

if command -v gfortran-12 >/dev/null 2>&1; then
	FPROG=getshells-fortran
	 gfortran-12 -o $FPROG getshells.f90
else
	echo "Fortran compiler not found."
fi

# Check for Powershell
if command -v pwsh >/dev/null 2>&1; then
	PSHELL=getshells.ps1
else
	echo "Powershell not found."
fi

# Check for PHP
if command -v php >/dev/null 2>&1; then
	PHP=getshells.php
else
	echo "PHP not found."
fi

# Check for awk
if command -v awk >/dev/null 2>&1; then
	AWK=getshells.awk
else
	echo "Awk not found."
fi

# Check for python
if command -v awk >/dev/null 2>&1; then
	PYPROG=getshells.py
else
	echo "Python3 not found."
fi

# Check for perl
if command -v perl >/dev/null 2>&1; then
	PLPROG=getshells.pl
	PLONEPROG=getshells-one-liner.pl
else
	echo "Perl not found."
fi

# Check for SBCL (Lisp)
if command -v sbcl >/dev/null 2>&1; then
	LISPPROG=getshells-lisp
	sbcl --dynamic-space-size 1024 \
		--load getshells.lisp \
		--eval '(sb-ext:save-lisp-and-die "getshells-lisp" :toplevel #'\''main :executable t :compression nil)' \
		--quit
else
	echo "SBCL (Lisp) not found."
fi

# Check for NodeJS
if command -v node >/dev/null 2>&1; then
	NODEPROG=getshells.js
else
	echo "NodeJS not found."
fi

# Check for julia
if command -v julia >/dev/null 2>&1; then
	JLPROG=getshells.jl
else
	echo "Julia not found."
fi

# Check for ruby
if command -v ruby >/dev/null 2>&1; then
	RBPROG=getshells.rb
else
	echo "Ruby not found."
fi

# Check for crystal
if command -v crystal >/dev/null 2>&1; then
	CRPROG=getshells-cr
	crystal build --release getshells.cr
	mv getshells ./${CRPROG}
else
	echo "Crystal-lang not found."
fi

# Check for Lua
if command -v lua >/dev/null 2>&1; then
	LUAPROG=getshells.lua
else
	echo "Lua not found."
fi

# Check for LuaJIT
if command -v luajit >/dev/null 2>&1; then
	LUAJITPROG=getshells.luajit
else
	echo "LuaJIT not found."
fi

# Check for Haskell compiler
if command -v ghc >/dev/null 2>&1; then
	HSPROG=getshells-hs
	ghc -o ${HSPROG} getshells.hs -no-keep-hi-files -no-keep-o-files
else
	echo "Haskell compiler not found."
fi

# Check for Elixir
if command -v elixir >/dev/null 2>&1; then
	EXSPROG=getshells.exs
else
	echo "Elixir not found."
fi

# Check for Clojure and/or babashka 
if command -v clojure >/dev/null 2>&1; then
	CLJPROG=getshells.clj
else
	echo "Clojure not found."
fi
if command -v bb >/dev/null 2>&1; then
	CLJPROG=getshells-bb.clj
else
	echo "Babashka not found."
fi

# Check for guile (scheme)
if command -v guile >/dev/null 2>&1; then
	SCMPROG=getshells.scm
	GUILEPROG=getshells-guile.scm
else
	echo "Guile (Scheme) not found."
fi

# Check for raku
if command -v raku >/dev/null 2>&1; then
	RKPROG=getshells.raku
else
	echo "Raku not found."
fi

# wrapper function for various time implementations so we can have memory readouts
# check bsd time first, then portable gnu time, gnu time, and finally fallback to posix time
run_benchmark() {
	if /usr/bin/time -l true >/dev/null 2>&1; then
		/usr/bin/time -l "$@"
	elif command -v gtime >/dev/null 2>&1; then
		gtime -f '%E\nMax RSS: %MK' "$@"
	elif /usr/bin/time -f '%E\nMax RSS: %MK' true >/dev/null 2>&1; then
		/usr/bin/time -f '%E\nMax RSS: %MK' "$@"
	else
		/usr/bin/time "$@"
	fi
}

LIST="${LUAPROG} ${LUAJITPROG} ${CPROG} ${RSPROG} ${GOPROG} ${NODEPROG} ${PYPROG} ${PLPROG} ${PLONEPROG} ${JLPROG} ${LISPPROG} ${RBPROG} ${AWK} ${CRPROG} ${PHP} ${HSPROG} ${RKPROG} ${PSHELL} ${EXSPROG} ${CLJPROG} ${SCMPROG} ${GUILEPROG} ${FPROG}"

for i in $LIST
do
	printf '%s\n' \
		'################################################' \
		"$i"

	run_benchmark "./$i"
done
