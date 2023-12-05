#!/bin/bash

if [ -z "$1" ]; then
	echo "No year specified"
	exit 1
fi

if [ -z "$2" ]; then
	echo "No day number specified"
	exit 1
fi

mkdir -p "./input/$1"

curl -sSLo "./input/$1/day$2.txt" "https://adventofcode.com/$1/day/$2/input"
