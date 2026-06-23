package main

import (
	"bufio"
	"bytes"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("passwd")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	us := make(map[string]int, 8)

	s := bufio.NewScanner(file)
	s.Buffer(make([]byte, 16*1024*1024), 16*1024*1024)
	for s.Scan() {
		line := s.Bytes()
		last := bytes.LastIndexByte(line, ':')
		if last < 0 {
			continue
		}
		us[string(line[last+1:])]++
	}

	for kk, vv := range us {
		fmt.Printf("%v:\t%v\n", kk, vv)
	}
}
