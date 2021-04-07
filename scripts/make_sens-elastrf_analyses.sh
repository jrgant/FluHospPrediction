#!/bin/bash

for t in "pkrate" "pkweek" "cumhosp"; do
  for b in "lambda-min" ; do
      sed -e "s/TARGET_INPUT/$t/g" -e "s/LAMBDA_INPUT/$b/g" -e "s/LEARNER_INPUT/'elast|rf'/g" submit_script_template_sens-elastrf.sh > RUN_TARGET-$t\_LAMBDA-$b\_SENS-ElastNetRF.sh
  done
done
