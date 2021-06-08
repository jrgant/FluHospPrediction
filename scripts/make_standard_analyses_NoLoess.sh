#!/bin/bash

for t in "pkrate" "pkweek" "cumhosp"; do
  for b in "lambda-min" "lambda-1se"; do
      sed -e "s/TARGET_INPUT/$t/g" -e "s/LAMBDA_INPUT/$b/g" -e "s/LEARNER_INPUT/'elast|lasso|ridge|glm|nnet|mars|rf|svm'/g" submit_script_template.sh > RUN_TARGET-$t\_LAMBDA-$b.sh
  done
done
