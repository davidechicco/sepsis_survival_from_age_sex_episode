#!/bin/bash
#
#$ -cwd
#$ -S /bin/bash
#
set -o nounset -o pipefail -o errexit
# set -o xtrace

today=`date +%Y-%m-%d`
random_number=$(shuf -i1-100000 -n1)
method="naive_bayes_regression"
jobName=$method"_"$today"_rand"$random_number
outputFile="../results/"$jobName

echo "The results will be printed into the "$outputFile" output file"

/usr/bin/Rscript naive_bayes_regression.r > $outputFile 2> $outputFile

echo "The end"

