#!/bin/bash

for t in "pkrate" "pkweek" "cumhosp"; do
  for b in "lambda-min" "lambda-1se"; do
      sed -e "s/TARGET_INPUT/$t/g" -e "s/LAMBDA_INPUT/$b/g" -e "s/LEARNER_INPUT/^lrnr/g" submit_script_template.sh > RUN_Target-$t\_Lambda-$b.sh
  done
done
