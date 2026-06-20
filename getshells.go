package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {

	file, err := os.Open("passwd")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var us = map[string]int{}

	s := bufio.NewScanner(file)
	for s.Scan() {
		linez := s.Text()
		result := strings.Split(linez, ":")
		// fmt.Println(result[0], result[6])
		us[result[6]] += 1;
	}

	for kk, vv := range us {
            fmt.Printf("%v:\t%v\n", kk, vv)
	}
}
