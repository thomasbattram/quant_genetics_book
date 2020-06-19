#!/bin/bash

cd notes/

chapters=$1

if [[ -z "$chapters" ]]; then
	echo "\$chapters is empty, making whole book"
	chapters="all"
fi


if [[ $chapters == "all" ]]; then
	# Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book')"
	Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
else 
	chap=0${chapters}-chapter${chapters}
	echo $chap
	# script="bookdown::preview_chapter('${chap}.Rmd')"
	# echo $script
	Rscript -e "bookdown::preview_chapter('${chap}.Rmd', output_format = 'all')"	
fi