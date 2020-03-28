#!/bin/bash

cd notes/

chapters=$1

if [[ $chapters == "all" ]]; then
	Rscript -e "require(knitr); require(markdown); require(bookdown); bookdown::render_book('index.Rmd')"	
else 
	chap=chapter${chapters}
	echo $chap
	# script="bookdown::preview_chapter('${chap}.Rmd')"
	# echo $script
	Rscript -e "bookdown::preview_chapter('${chap}.Rmd')"	
fi