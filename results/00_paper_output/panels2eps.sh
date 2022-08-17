#!/bin/bash
for FILE in $(ls *Panel*pdf); 
  do pdf2ps $FILE; 
done;
rename 's/ps/eps/' *.ps -v
