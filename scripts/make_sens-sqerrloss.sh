#!/bin/bash

for t in "pkrate" "pkweek" "cumhosp"; do
  for b in "lambda-min" ; do
      sed -e "s/TARGET_INPUT/$t/g" -e "s/LAMBDA_INPUT/$b/g" -e "s/LEARNER_INPUT/'elast|lasso|ridge|glm|nnet|mars|rf|svm'/g" submit_script_template_sens-sqerrloss.sh > RUN_TARGET-$t\_LAMBDA-$b\_SENS-SqErrLoss.sh
  done
done
