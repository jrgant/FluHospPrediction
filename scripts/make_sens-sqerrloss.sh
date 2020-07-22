#!/bin/bash

for t in "pkrate" "pkweek" "cumhosp"; do
  for b in "lambda-min" ; do
      sed -e "s/TARGET_INPUT/$t/g" -e "s/LAMBDA_INPUT/$b/g" -e "s/LEARNER_INPUT/^lrnr/g" submit_script_template_sens-sqerrloss.sh > RUN_Target-$t\_Lambda-$b\_Sens-SqErrLoss.sh
  done
done
