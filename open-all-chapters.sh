#!/bin/bash

cd notes/

ls | grep "[0-9][0-9]-chapter.*" > tmp.txt

while read id; do
	echo "$id"
	subl $id
done <tmp.txt

rm tmp.txt

cd ..