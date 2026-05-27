#!/bin/sh

# Check for standalone time
if [ -z "$TIME" ]; then
	TIME="$(which time 2>/dev/null)"
fi
if [ -z "$TIME" ]; then
	echo "Failed to find standalone time executable"
	exit
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

if [ -n "${CC}" ]; then
	CPROG=getshells-c
	${CC} -O3 getshells.c -o ${CPROG}
else
	echo "C Compiler not found."
fi

# Check for rust compiler
if [ -n "$(which cargo 2>/dev/null)" ]; then
	RSPROG="getshells-rust"
	cd "getshells_rust" || echo "getshells_rust folder not found"
	cargo build --release --all-features
	cd ".."
 	cp getshells_rust/target/release/getshells ${RSPROG}
else
	echo "cargo was not found"
fi

# Check for golang compiler
if [ -n "$(which go 2>/dev/null)" ]; then # literal string will always return true value
	GOPROG=getshells-go
	go build getshells.go
	mv getshells ${GOPROG}
else
	echo "Golang compiler not found."
fi

# Check for Powershell
if [ -n "$(which pwsh 2>/dev/null)" ]; then
	PSHELL=getshells.ps1
else
	echo "Powershell not found."
fi

# Check for PHP
if [ -n "$(which php 2>/dev/null)" ]; then
	PHP=getshells.php
else
	echo "PHP not found."
fi

# Check for awk
if [ -n "$(which awk 2>/dev/null)" ]; then
	AWK=getshells.awk
else
	echo "Awk not found."
fi

if [ -n "$(which python3 2>/dev/null)" ]; then
	PYPROG=getshells.py
else
	echo "Python3 not found."
fi

if [ -n "$(which perl 2>/dev/null)" ]; then
	PLPROG=getshells.pl
else
	echo "Perl not found."
fi

if [ -n "$(which sbcl 2>/dev/null)" ]; then
	LISPPROG=getshells.lisp
else
	echo "SBCL (Lisp) not found."
fi

if [ -n "$(which node 2>/dev/null)" ]; then
	NODEPROG=getshells.js
else
	echo "NodeJS not found."
fi

if [ -n "$(which julia 2>/dev/null)" ]; then
	JLPROG=getshells.jl
else
	echo "Julia not found."
fi

if [ -n "$(which ruby 2>/dev/null)" ]; then
	RBPROG=getshells.rb
else
	echo "Ruby not found."
fi

if [ -n "$(which crystal 2>/dev/null)" ]; then
	CRPROG=getshells-cr
	crystal build --release getshells.cr
	mv getshells ./${CRPROG}
else
	echo "Crystal-lang not found."
fi

# Check for Lua
if [ -n "$(which lua 2>/dev/null)" ]; then
	LUA=getshells.lua
else
	echo "Lua not found."
fi

# Check for LuaJIT
if [ -n "$(which luajit 2>/dev/null)" ]; then
	LUA=getshells.luajit
else
	echo "LuaJIT not found."
fi

# Check for Haskell compiler
if [ -n "$(which ghc 2>/dev/null)" ]; then
	HSPROG=getshells-hs
	ghc -o ${HSPROG} getshells.hs -no-keep-hi-files -no-keep-o-files
else
	echo "Haskell compiler not found."
fi

if [ -n "$(which elixir 2>/dev/null)" ]; then
    EXSPROG=getshells.exs
else
    echo "Elixir not found."
fi

LIST="${LUA} ${CPROG} ${RSPROG} ${GOPROG} ${NODEPROG} ${PYPROG} ${PLPROG} ${JLPROG} ${LISPPROG} ${RBPROG} ${AWK} ${CRPROG} ${PHP} ${HSPROG} ${PSHELL} ${EXSPROG}"

for i in ${LIST} ; do
    echo "################################################"
    echo "$i"
    $TIME -f "%E\nMax memory usage: %MK" "./${i}"
done
