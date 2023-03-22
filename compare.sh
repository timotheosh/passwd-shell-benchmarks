#!/bin/sh

if ! [ "$SHELL" = "bash" ]; then
# Check for standalone time
TIME="$(which time 2>/dev/null)"
if [ -z "$TIME" ]; then
	echo "Failed to find standalone time executable"
	exit
fi
fi

# Check for C Compiler
if [ -n "${CC}" ]; then
	# CC=${CC} # why are you assigning $CC to itsself
	: # Do nothing, since $CC was defined
elif [ -n "$(which clang 2>/dev/null)" ]; then
	CC=clang
elif [ -n "$(which gcc 2>/dev/null)" ]; then
	CC=gcc
elif [ -n "$(which cc 2>/dev/null)" ]; then
	CC=cc
fi

# Check for C++ Compiler
if [ -n "${CXX}" ]; then
	# CXX=${CXX} # why are you assigning $CC to itsself
	: # Do nothing, since $CC was defined
elif [ -n "$(which clang++ 2>/dev/null)" ]; then
	CXX=clang++
elif [ -n "$(which g++ 2>/dev/null)" ]; then
	CXX=g++
elif [ -n "$(which c++ 2>/dev/null)" ]; then
	CXX=c++
fi

if [ -n "${CC}" ]; then
	CPROG=getshells-c
	CPROG_HYPER="./getshells-c -n C"
	${CC} -march=native -Ofast -mtune=native -pthread getshells.c -o ${CPROG}
else
	echo "C Compiler not found."
fi

if [ -n "${CXX}" ]; then
	CPPPROG=getshells-cpp
	CPPPROG_HYPER="./getshells-cpp -n C++"
	${CXX} -march=native -O3 getshells.cpp -o ${CPPPROG}
else
	echo "C++ Compiler not found."
fi

# Check for rust compiler
if [ -n "$(which cargo 2>/dev/null)" ]; then
	export RUSTFLAGS="-C target-cpu=native"
	RSPROG="release/getshells"
	RSPROG_HYPER="./release/getshells -n Rust"
	cd "getshells_rust" || echo "getshells_rust folder not found"
	cargo build --release --all-features --target-dir ..
	cd ".."
	M_RSPROG="release/getshells_multi release/multireader"
	M_RSPROG_HYPER="./release/getshells_multi -n Rust(multithread) ./release/multireader -n Rust(better_multithreaded)"
	cd "getshells_multi" || echo "getshells_multi folder not found"
	cargo build --release --all-features --target-dir ..
	cd ".."
else
	echo "cargo was not found"
fi

# Check for golang compiler
if [ -n "$(which go 2>/dev/null)" ]; then # literal string will always return true value
	GOPROG=getshells-go
	GOPROG_HYPER="./getshells-go -n Go"
	go build getshells.go
	mv getshells ${GOPROG}
else
	echo "Golang compiler not found."
fi

# Check for Powershell
if [ -n "$(which pwsh 2>/dev/null)" ]; then
	PSHELL=getshells.ps1
	PSHELL_HYPER="./getshells.ps1 -n PowerShell"
else
	echo "Powershell not found."
fi

# Check for awk
if [ -n "$(which awk 2>/dev/null)" ]; then
	AWK=getshells.awk
	AWK_HYPER="./getshells.awk -n Awk"
else
	echo "Awk not found."
fi

if [ -n "$(which python3 2>/dev/null)" ]; then
	PYPROG=getshells.py
	PYPROG_HYPER="./getshells.py -n Python"
else
	echo "Python3 not found."
fi

if [ -n "$(which perl 2>/dev/null)" ]; then
	PLPROG=getshells.pl
	PLPROG_HYPER="./getshells.pl -n Perl"
else
	echo "Perl not found."
fi

if [ -n "$(which sbcl 2>/dev/null)" ]; then
	LISPPROG=getshells.lisp
	LISPPROG_HYPER="./getshells.lisp -n LISP"
else
	echo "SBCL (Lisp) not found."
fi

if [ -n "$(which node 2>/dev/null)" ]; then
	NODEPROG=getshells.js
	NODEPROG_HYPER="./getshells.js -n Node"
else
	echo "NodeJS not found."
fi

if [ -n "$(which julia 2>/dev/null)" ]; then
	JLPROG=getshells.jl
	JLPROG_HYPER="./getshells.jl -n Julia"
else
	echo "Julia not found."
fi

if [ -n "$(which ruby 2>/dev/null)" ]; then
	RBPROG=getshells.rb
	RBPROG_HYPER="./getshells.rb -n Ruby"
else
	echo "Ruby not found."
fi

if [ -n "$(which crystal 2>/dev/null)" ]; then
	CRPROG=getshells-cr
	CRPROG_HYPER="./getshells-cr -n Crystal"
	M_CRPROG=getshells-m-cr
	M_CRPROG_HYPER="./getshells-m-cr -n Crystal(multithread)" 
	crystal build --stats --progress --time --mcpu=native --verbose --release getshells_m.cr
	crystal build --stats --progress --time --mcpu=native --verbose --release getshells.cr
	mv getshells ./${CRPROG}
	mv getshells_m ./${M_CRPROG}
else
	echo "Crystal-lang not found."
fi

# Check for Lua
if [ -n "$(which lua 2>/dev/null)" ]; then
	LUA=getshells.lua
	LUA_HYPER="./getshells.lua -n Lua"
else
	echo "Lua not found."
fi

# Check for LuaJIT
if [ -n "$(which luajit 2>/dev/null)" ]; then
	LUAJIT=getshells.luajit
	LUAJIT_HYPER="./getshells.luajit -n LuaJIT"
else
	echo "LuaJIT not found."
fi

if [ -n "$(which ghc 2>/dev/null)" ]; then
	HSPROG=getshells-hs
	HSPROG_HYPER="./getshells-hs -n Haskell"
	ghc -O3 -threaded getshells.hs -o ${HSPROG} -fforce-recomp -outputdir=/tmp
else
	echo "ghc (Haskell compiler) not found."
fi

LIST="${LUA} ${LUAJIT} ${CPROG} ${CPPPROG} ${RSPROG} ${M_RSPROG} ${GOPROG} ${NODEPROG} ${PYPROG} ${PLPROG} ${JLPROG} ${RBPROG} ${CRPROG} ${M_CRPROG} ${PLPROG} ${AWK} ${LISPPROG} ${PSHELL} ${HSPROG}"
LIST_HYPER="${LUA_HYPER} ${LUAJIT_HYPER} ${CPROG_HYPER} ${CPPPROG_HYPER} ${RSPROG_HYPER} ${M_RSPROG_HYPER} ${GOPROG_HYPER} ${NODEPROG_HYPER} ${PYPROG_HYPER} ${JLPROG_HYPER} ${RBPROG_HYPER} ${CRPROG_HYPER} ${M_CRPROG_HYPER} ${PLPROG_HYPER} ${AWK_HYPER} ${LISPPROG_HYPER} ${PSHELL_HYPER} ${HSPROG_HYPER}"

if [ -n "$(which hyperfine 2>/dev/null)" ]; then
	echo "Found hyperfine, using it to benchmark"

	hyperfine -i $LIST_HYPER --warmup 5 -N
else
	echo "Hyperfine not found, using rudimentary benchmarking"

	for i in ${LIST}; do
		echo "################################################"
		echo "$i"
		$TIME -f "%E\nMax memory usage: %MK" "./${i}"
	done
fi
